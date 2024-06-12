/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.batch;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.item.ItemWriter;
import org.springframework.beans.factory.annotation.Autowired;

import com.acm.client.CreditClient;
import com.acm.client.TransversClient;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.CalculateAgeException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.AcmDamageDataService;
import com.acm.service.AcmEnvironnementService;
import com.acm.service.UserDefinedFieldsService;
import com.acm.utils.dtos.AcmDamagedDataDTO;
import com.acm.utils.dtos.AddressDTO;
import com.acm.utils.dtos.CustomerDTO;
import com.acm.utils.dtos.CustomerLinksRelationshipDTO;
import com.acm.utils.dtos.CustomerMemberDTO;
import com.acm.utils.dtos.UserDefinedFieldsDTO;
import com.acm.utils.dtos.UserDefinedFieldsLinksDTO;
import com.acm.utils.enums.CustomerType;
import com.acm.utils.enums.LinkRelationshipsCategory;
import com.acm.utils.models.Customer;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;

/**
 * The Class CustomerWriter.
 */
public class CustomerWriter implements ItemWriter<CustomerDTO> {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(CustomerWriter.class);

	/** The credit client. */
	@Autowired
	private CreditClient creditClient;

	/** The transvers client. */
	@Autowired
	private TransversClient transversClient;

	/** The user defined fields service. */
	@Autowired
	private UserDefinedFieldsService userDefinedFieldsService;

	/** The acm environnement service. */
	@Autowired
	private AcmEnvironnementService acmEnvironnementService;

	/** The acm damage customer service. */
	@Autowired
	private AcmDamageDataService acmDamageCustomerService;

	/*
	 * (non-Javadoc)
	 * @see org.springframework.batch.item.ItemWriter#write(java.util.List)
	 */
	@Override
	public void write(List<? extends CustomerDTO> items) throws Exception {

		if (!ACMValidationUtils.isNullOrEmpty(items)) {
			for (CustomerDTO item : items) {
				try {

					CustomerDTO newCustomerDTO = creditClient.createCustomer(item);
					try {
						// get udf customer from abacus

						List<UserDefinedFieldsLinksDTO> listUserDefinedFieldsLinksAbacus =
								transversClient
										.loadUDFByCustomer(newCustomerDTO.getCustomerIdExtern());
						logger.debug("#############  Customer UDF {} #####################",
								listUserDefinedFieldsLinksAbacus);
						// if customer udf not empty
						if (!ACMValidationUtils.isNullOrEmpty(listUserDefinedFieldsLinksAbacus)) {
							for (UserDefinedFieldsLinksDTO userDefinedFieldsLinksAbacus : listUserDefinedFieldsLinksAbacus) {
								List<UserDefinedFieldsDTO> userDefinedFieldsDTOs =
										userDefinedFieldsService.find(userDefinedFieldsLinksAbacus
												.getUserDefinedFieldsDTO());
								if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldsDTOs)) {
									userDefinedFieldsLinksAbacus
											.setUserDefinedFieldsDTO(userDefinedFieldsDTOs.get(0));
								}
							}
						}
						// save UDF in ACM
						for (UserDefinedFieldsLinksDTO userDefinedFieldsLinksACM : listUserDefinedFieldsLinksAbacus) {
							creditClient.createByBatch(userDefinedFieldsLinksACM);
						}
					}
					catch (Exception e) {
						logger.error(
								" ################# cannot add UDF For customer #################");
						AcmDamagedDataDTO acmDamagedCustomerDTO = acmDamageCustomerService
								.save(new AcmDamagedDataDTO(newCustomerDTO.getCustomerIdExtern(),
										"BLOCK ADD UDF CUSTOMER", e.getCause().getMessage()));
						logger.debug(
								"#############  Customer saved in ACM_DAMAGED_CUSTOMER {} #####################",
								acmDamagedCustomerDTO);
						newCustomerDTO.setEnabled(Boolean.FALSE);
						creditClient.update(newCustomerDTO);
					}

					try {
						// get address list from abacus
						List<AddressDTO> listAddressAbacus = transversClient
								.loadAddressByCustomer(newCustomerDTO.getCustomerIdExtern());
						logger.debug("#############  Customer ADDRESS {} #####################",
								listAddressAbacus);

						// if customer address list not empty
						if (!ACMValidationUtils.isNullOrEmpty(listAddressAbacus)) {
							for (AddressDTO addressDTO : listAddressAbacus) {
								addressDTO.setCustomerId(newCustomerDTO.getCustomerIdExtern());
								creditClient.createByBatch(addressDTO);
							}
						}
					}
					catch (Exception e) {
						logger.error(
								"################# cannot add address customer #################");
						AcmDamagedDataDTO acmDamagedCustomerDTO = acmDamageCustomerService
								.save(new AcmDamagedDataDTO(newCustomerDTO.getCustomerIdExtern(),
										"BLOCK ADD ADDRESS CUSTOMER", e.getCause().getMessage()));
						logger.debug(
								"#############  Customer saved in ACM_DAMAGED_CUSTOMER {} #####################",
								acmDamagedCustomerDTO);
						newCustomerDTO.setEnabled(Boolean.FALSE);
						creditClient.update(newCustomerDTO);
					}

					try {
						// relationship
						loadAndSettingMembers(newCustomerDTO);
					}
					catch (Exception e) {
						logger.error(
								" ################# cannot Relationship forcustomer ################# ");
						AcmDamagedDataDTO acmDamagedCustomerDTO = acmDamageCustomerService
								.save(new AcmDamagedDataDTO(newCustomerDTO.getCustomerIdExtern(),
										"BLOCK ADD RELATIONSHIP CUSTOMER",
										e.getCause().getMessage()));
						logger.debug(
								"#############  Customer saved in ACM_DAMAGED_CUSTOMER {} #####################",
								acmDamagedCustomerDTO);

						newCustomerDTO.setEnabled(Boolean.FALSE);
						creditClient.update(newCustomerDTO);
					}
				}
				catch (Exception e) {
					logger.error(" ################# cannot add customer ################# ");
					AcmDamagedDataDTO acmDamagedCustomerDTO = acmDamageCustomerService
							.save(new AcmDamagedDataDTO(item.getCustomerIdExtern(),
									"BLOCK ADD CUSTOMER ", e.getCause().getMessage()));
					logger.debug(
							"#############  Customer saved in ACM_DAMAGED_CUSTOMER {} #####################",
							acmDamagedCustomerDTO);

				}
				// update acm environnement
				// update limite if loan exit
				acmEnvironnementService.updateLimite("LIMITE_ID_CUSTOMER_EXTERNE",
						String.valueOf(item.getCustomerIdExtern()));
				logger.debug(
						"#############  Updated LIMITE_ID_CUSTOMER_EXTERNE #####################");
			}

			// saving data in DB
			logger.debug("#############  Writer Customer {} #####################", items);
		}

	}

	/**
	 * Load and setting members.
	 *
	 * @param item the item
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CalculateAgeException the calculate age exception
	 */
	private void loadAndSettingMembers(CustomerDTO item)
			throws ResourcesNotFoundException, CalculateAgeException {

		// init list
		List<CustomerMemberDTO> customerMemberDTOs = new ArrayList<>();

		// setting members if customer type is GRP or ORG
		if (item.getCustomerType().equals(CustomerType.GRP.name())) {
			// load members if customer type = GRP
			customerMemberDTOs =
					transversClient.findMembersGroupByCustomer(item.getCustomerIdExtern());
		}
		else if (item.getCustomerType().equals(CustomerType.ORG.name())) {
			// load members if customer type = ORG
			customerMemberDTOs =
					transversClient.findMembersOrganisationByCustomer(item.getCustomerIdExtern());
		}

		// inserting group members
		for (CustomerMemberDTO customerMemberDTO : customerMemberDTOs) {
			// check member if exist before saving link
			CustomerDTO member = loadCustomer(customerMemberDTO.getCustomerId());

			// check link customer members if exist
			CustomerLinksRelationshipDTO params = new CustomerLinksRelationshipDTO();
			params.setCustomerId(item.getId());
			params.setMember(member);
			params.setCategory(LinkRelationshipsCategory.MEMBERS.name());
			List<CustomerLinksRelationshipDTO> existMembre = creditClient.find(params);

			// check if member is not registered
			if (ACMValidationUtils.isNullOrEmpty(existMembre)) {
				// saving member in DB
				CustomerLinksRelationshipDTO newMember = creditClient
						.create(new CustomerLinksRelationshipDTO(item.getId(), member, null,
								LinkRelationshipsCategory.MEMBERS.name(), new Date(), null, null));
				logger.info("newMember = {}", newMember);
			}
			// ???????
			// saving link in DB
			CustomerLinksRelationshipDTO newLink =
					creditClient.create(new CustomerLinksRelationshipDTO(item.getId(), member,
							customerMemberDTO.getCustomerRole(),
							LinkRelationshipsCategory.LINK.name(), new Date(), null,
							existMembre.get(0).getPercentageOwned()));
			logger.info("newLink = {}", newLink);
		}

		// load Relationship for Customer
		List<CustomerMemberDTO> customerRelationship =
				transversClient.findRelationshipByCustomer(item.getCustomerIdExtern());
		// inserting Relationship
		for (CustomerMemberDTO customerMemberDTO : customerRelationship) {
			// check Relationship if exist before saving link
			CustomerDTO relationship = loadCustomer(customerMemberDTO.getRelationshipId());

			// check link customer relationship if exist
			CustomerLinksRelationshipDTO params = new CustomerLinksRelationshipDTO();
			params.setCustomerId(item.getId());
			params.setMember(relationship);
			params.setCategory(LinkRelationshipsCategory.RELATIONSHIP.name());
			List<CustomerLinksRelationshipDTO> existRelationship = creditClient.find(params);
			if (!ACMValidationUtils.isNullOrEmpty(existRelationship)) {
				// update existing relationship => Add ID_ACM_LOAN
				CustomerLinksRelationshipDTO customerLinksRelationshipDTO =
						existRelationship.get(0);
				// update data
				creditClient.update(customerLinksRelationshipDTO);
			}
			else {
				// saving Relationship in DB
				CustomerLinksRelationshipDTO newRelationship =
						creditClient.create(new CustomerLinksRelationshipDTO(item.getId(),
								relationship, customerMemberDTO.getCustomerRole(),
								LinkRelationshipsCategory.RELATIONSHIP.name(), new Date(), null,
								null));
				logger.info("newRelationship = {}", newRelationship);
			}
		}
	}

	/**
	 * Load customer.
	 *
	 * @param customerIdExtern the customer id extern
	 * @return the customer DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	private CustomerDTO loadCustomer(Long customerIdExtern) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(customerIdExtern, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);

		// check if customer if exist in ACM DB by customerIdExtern
		List<CustomerDTO> customerDTOs = creditClient.findCustomerIdExtern(customerIdExtern);
		if (!ACMValidationUtils.isNullOrEmpty(customerDTOs)) {
			return customerDTOs.get(0);
		}
		else {
			// load Customer from ABACUS Data
			CustomerDTO customerDTOAbacus = transversClient.findCustomerById(customerIdExtern);
			logger.info("{}", customerDTOAbacus);
			if (ACMValidationUtils.isNullOrEmpty(customerDTOAbacus)
					|| ACMValidationUtils.isNullOrEmpty(customerDTOAbacus.getCustomerIdExtern())) {
				logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Customer.class.getSimpleName());
				throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
						CommonExceptionsMessage.NOT_FOUND + Customer.class.getSimpleName()
								+ CommonExceptionsMessage.WITH_ID + customerIdExtern);
			}
			// setting CustomerName
			if (customerDTOAbacus.getCustomerType().equals(CustomerType.GRP.name())) {
				customerDTOAbacus.setCustomerName(customerDTOAbacus.getCorrespondanceName());
				customerDTOAbacus.setSolidarityName(customerDTOAbacus.getCorrespondanceName());
			}
			else if (customerDTOAbacus.getCustomerType().equals(CustomerType.ORG.name())) {
				customerDTOAbacus.setCustomerName(customerDTOAbacus.getCorrespondanceName());
				customerDTOAbacus.setOrganizationName(customerDTOAbacus.getCorrespondanceName());
			}
			// insert customer in ACM DB
			CustomerDTO newCustomerDTO = creditClient.createCustomer(customerDTOAbacus);

			logger.info("new customer* with ID = {} / NUMBER = {} has been add in ACM.",
					newCustomerDTO.getId(), newCustomerDTO.getCustomerNumber());
			return newCustomerDTO;
		}
	}

}
