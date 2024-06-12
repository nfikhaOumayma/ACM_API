/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Service;

import com.acm.client.UserClient;
import com.acm.constants.common.ACMConstantWorkflowStatuts;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.CustomerDesicionRepository;
import com.acm.service.CustomerDecisionService;
import com.acm.utils.dtos.AcmStatutsDTO;
import com.acm.utils.dtos.CustomerDecisionDTO;
import com.acm.utils.models.CustomerDecision;
import com.acm.utils.models.Loan;
import com.acm.utils.models.QCustomerDecision;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link CustomerDesicionServiceImpl} CustomerDesicionServiceImpl.
 *
 * @author YesserSOmai
 * @since 0.2.0
 */
@Service
public class CustomerDesicionServiceImpl implements CustomerDecisionService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(CustomerDesicionServiceImpl.class);

	/** The customer desicion repository. */
	@Autowired
	private CustomerDesicionRepository customerDesicionRepository;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The Environment. */
	@Autowired
	private Environment environment;

	/** The user Client client. */
	@Autowired
	private UserClient userClient;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerDesicionService#find(java.lang.Long)
	 */
	@Override
	public CustomerDecisionDTO find(Long id) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find CustomerDecision by ID : {}", id);
		CustomerDecision customerDesicion = customerDesicionRepository.findById(id).orElse(null);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(customerDesicion)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					CustomerDecision.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty("exception.message.not.found")
							+ CustomerDecision.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		return mapper.map(customerDesicion, CustomerDecisionDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerDesicionService#find(com.acm.utils.dtos.CustomerDesicionDTO)
	 */
	@Override
	public List<CustomerDecisionDTO> find(CustomerDecisionDTO customerDesicionDTO) {

		Preconditions.checkNotNull(customerDesicionDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Preconditions.checkNotNull(customerDesicionDTO.getIdLoan(),
				CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		// init QCustomerDecision
		QCustomerDecision qCustomerDesicion = QCustomerDecision.customerDecision;

		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find only enabled data
		predicate.and(qCustomerDesicion.enabled.eq(Boolean.TRUE));

		// find by Loan (required)
		predicate.and(qCustomerDesicion.loan.eq(new Loan(customerDesicionDTO.getIdLoan())));

		// find by contactDate
		if (!ACMValidationUtils.isNullOrEmpty(customerDesicionDTO.getContactDate())) {
			predicate.and(qCustomerDesicion.contactDate.eq(customerDesicionDTO.getContactDate()));
		}

		Iterable<CustomerDecision> iterable = customerDesicionRepository.findAll(predicate);
		List<CustomerDecision> customerDesicions = new ArrayList<>();
		iterable.forEach(customerDesicions::add);
		logger.info("{} : Loan was founded", customerDesicions.size());

		List<CustomerDecisionDTO> customerDesicionsDTOs = new ArrayList<>();
		customerDesicions.forEach(customerDesicion -> customerDesicionsDTOs
				.add(mapper.map(customerDesicion, CustomerDecisionDTO.class)));
		return customerDesicionsDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerDesicionService#save(com.acm.utils.dtos.CustomerDesicionDTO)
	 */
	@Override
	public CustomerDecisionDTO save(CustomerDecisionDTO customerDesicionDTO) {

		Preconditions.checkNotNull(customerDesicionDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		CustomerDecision customerDesicion = mapper.map(customerDesicionDTO, CustomerDecision.class);
		// setting loan by id
		customerDesicion.setLoan(new Loan(customerDesicionDTO.getIdLoan()));

		// setting status
		AcmStatutsDTO acmStatutsDTO = CommonFunctions.loadStatus(customerDesicion.getStatusId(),
				Arrays.asList(
						CommonFunctions.mappingStatus(
								ACMConstantWorkflowStatuts.CUSTOMER_DESICION_STATUS_ASK_REVIEW),
						CommonFunctions.mappingStatus(
								ACMConstantWorkflowStatuts.CUSTOMER_DESICION_STATUS_AGREED),
						CommonFunctions.mappingStatus(
								ACMConstantWorkflowStatuts.CUSTOMER_DESICION_STATUS_NOTE),
						CommonFunctions.mappingStatus(
								ACMConstantWorkflowStatuts.CUSTOMER_DESICION_STATUS_DECLIEND),
						CommonFunctions.mappingStatus(
								ACMConstantWorkflowStatuts.CUSTOMER_DESICION_REVIEW_AGREEMENTS)));
		customerDesicion.setStatusLibelle(acmStatutsDTO.getValue());
		CommonFunctions.mapperToSave(customerDesicion, userClient, logger);
		CustomerDecision newCustomerDesicion = customerDesicionRepository.save(customerDesicion);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, CustomerDecision.class.getSimpleName());
		return mapper.map(newCustomerDesicion, CustomerDecisionDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerDesicionService#save(java.lang.Long,
	 * com.acm.utils.dtos.CustomerDesicionDTO)
	 */
	@Override
	public CustomerDecisionDTO save(Long id, CustomerDecisionDTO customerDecisionDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(customerDecisionDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Update loan  with ID = {}", id);
		CustomerDecision oldCustomerDesicion = customerDesicionRepository.findById(id).orElse(null);

		// check if object is null
		if (oldCustomerDesicion == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					CustomerDecision.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + CustomerDecision.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		// mapping new data with existing data (oldCustomerDesicion)
		mapper.map(customerDecisionDTO, oldCustomerDesicion);
		CommonFunctions.mapperToUpdate(oldCustomerDesicion, userClient, logger);

		// setting status
		AcmStatutsDTO acmStatutsDTO = CommonFunctions.loadStatus(oldCustomerDesicion.getStatusId(),
				Arrays.asList(
						CommonFunctions.mappingStatus(
								ACMConstantWorkflowStatuts.CUSTOMER_DESICION_STATUS_ASK_REVIEW),
						CommonFunctions.mappingStatus(
								ACMConstantWorkflowStatuts.CUSTOMER_DESICION_STATUS_NOTE),
						CommonFunctions.mappingStatus(
								ACMConstantWorkflowStatuts.CUSTOMER_DESICION_STATUS_AGREED),
						CommonFunctions.mappingStatus(
								ACMConstantWorkflowStatuts.CUSTOMER_DESICION_STATUS_DECLIEND),
						CommonFunctions.mappingStatus(
								ACMConstantWorkflowStatuts.CUSTOMER_DESICION_REVIEW_AGREEMENTS)));
		oldCustomerDesicion.setStatusLibelle(acmStatutsDTO.getValue());

		// update & persist data in DB
		CustomerDecision newAcmCustomerDesicion =
				customerDesicionRepository.save(oldCustomerDesicion);

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, CustomerDecision.class.getSimpleName());
		return mapper.map(newAcmCustomerDesicion, CustomerDecisionDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CustomerDesicionService#validate(com.acm.utils.dtos.CustomerDesicionDTO)
	 */
	@Override
	public CustomerDecisionDTO validate(CustomerDecisionDTO customerDesicionDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(customerDesicionDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		CustomerDecisionDTO updatedCustomerDesicionDTO = null;
		if (customerDesicionDTO.getStatusId() != null) {
			switch (customerDesicionDTO.getStatusId()) {
				case 1:
					customerDesicionDTO.setStatusLibelle(CommonFunctions
							.mappingStatus(
									ACMConstantWorkflowStatuts.CUSTOMER_DESICION_STATUS_ASK_REVIEW)
							.getValue());
					// update status CustomerDesicion in DB
					updatedCustomerDesicionDTO =
							save(customerDesicionDTO.getId(), customerDesicionDTO);
					logger.info(
							"update status of CustomerDesicion with id {} to CUSTOMER_DESICION_STATUS_ASK_REVIEW :: DONE",
							updatedCustomerDesicionDTO.getStatusId());
					// returning data
					return updatedCustomerDesicionDTO;

				case 2:
					customerDesicionDTO.setStatusLibelle(CommonFunctions
							.mappingStatus(
									ACMConstantWorkflowStatuts.CUSTOMER_DESICION_STATUS_DECLIEND)
							.getValue());
					// update status CustomerDesicion in DB
					updatedCustomerDesicionDTO =
							save(customerDesicionDTO.getId(), customerDesicionDTO);
					logger.info(
							"update status of CustomerDesicion with id {} to CUSTOMER_DESICION_STATUS_DECLIEND :: DONE",
							updatedCustomerDesicionDTO.getStatusId());
					// returning data
					return updatedCustomerDesicionDTO;

				case 3:
					customerDesicionDTO.setStatusLibelle(CommonFunctions
							.mappingStatus(
									ACMConstantWorkflowStatuts.CUSTOMER_DESICION_STATUS_AGREED)
							.getValue());
					// update status CustomerDesicion in DB
					updatedCustomerDesicionDTO =
							save(customerDesicionDTO.getId(), customerDesicionDTO);
					logger.info(
							"update status of CustomerDesicion with id {} to CUSTOMER_DESICION_STATUS_AGREED :: DONE",
							updatedCustomerDesicionDTO.getStatusId());
					// returning data
					return updatedCustomerDesicionDTO;

				default:
					break;
			}
		}
		return updatedCustomerDesicionDTO;
	}

	/* (non-Javadoc)
	 * @see com.acm.service.CustomerDecisionService#save(com.acm.utils.dtos.CustomerDecisionDTO, java.lang.String)
	 */
	@Override
	public CustomerDecisionDTO save(CustomerDecisionDTO customerDesicionDTO, String InsertBy) {

		Preconditions.checkNotNull(customerDesicionDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		CustomerDecision customerDesicion = mapper.map(customerDesicionDTO, CustomerDecision.class);
		// setting loan by id
		customerDesicion.setLoan(new Loan(customerDesicionDTO.getIdLoan()));

		// setting status
		AcmStatutsDTO acmStatutsDTO = CommonFunctions.loadStatus(customerDesicion.getStatusId(),
				Arrays.asList(
						CommonFunctions.mappingStatus(
								ACMConstantWorkflowStatuts.CUSTOMER_DESICION_STATUS_ASK_REVIEW),
						CommonFunctions.mappingStatus(
								ACMConstantWorkflowStatuts.CUSTOMER_DESICION_STATUS_AGREED),
						CommonFunctions.mappingStatus(
								ACMConstantWorkflowStatuts.CUSTOMER_DESICION_STATUS_NOTE),
						CommonFunctions.mappingStatus(
								ACMConstantWorkflowStatuts.CUSTOMER_DESICION_STATUS_DECLIEND),
						CommonFunctions.mappingStatus(
								ACMConstantWorkflowStatuts.CUSTOMER_DESICION_REVIEW_AGREEMENTS)));
		customerDesicion.setStatusLibelle(acmStatutsDTO.getValue());
		CommonFunctions.mapperToSave(customerDesicion, userClient, logger);
		customerDesicion.setInsertBy(InsertBy);
		CustomerDecision newCustomerDesicion = customerDesicionRepository.save(customerDesicion);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, CustomerDecision.class.getSimpleName());
		return mapper.map(newCustomerDesicion, CustomerDecisionDTO.class);
	}
}
