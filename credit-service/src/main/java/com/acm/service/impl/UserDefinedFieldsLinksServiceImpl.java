/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import javax.persistence.EntityManager;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.stereotype.Service;

import com.acm.client.ParametrageClient;
import com.acm.client.TransversClient;
import com.acm.client.UserClient;
import com.acm.constants.common.CommonConstants;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.UserDefinedFieldsLinksRepository;
import com.acm.service.CustomerService;
import com.acm.service.LoanService;
import com.acm.service.UserDefinedFieldsLinksService;
import com.acm.utils.dtos.CustomerDTO;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.LoansUdfDTO;
import com.acm.utils.dtos.UDFLinksGroupeFieldsDTO;
import com.acm.utils.dtos.UDFLinksGroupeFieldsModelDTO;
import com.acm.utils.dtos.UserDefinedFieldGroupDTO;
import com.acm.utils.dtos.UserDefinedFieldListValuesDTO;
import com.acm.utils.dtos.UserDefinedFieldsDTO;
import com.acm.utils.dtos.UserDefinedFieldsLinksDTO;
import com.acm.utils.models.QUserDefinedFieldsLinks;
import com.acm.utils.models.UserDefinedFields;
import com.acm.utils.models.UserDefinedFieldsLinks;
import com.acm.utils.number.NumberUtils;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link UserDefinedFieldsLinksServiceImpl} Class Impl.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
@Service
public class UserDefinedFieldsLinksServiceImpl implements UserDefinedFieldsLinksService {

	/** The Constant logger. */
	private static final Logger logger =
			LoggerFactory.getLogger(UserDefinedFieldsLinksServiceImpl.class);

	/** The userDefinedFieldsLinks repository. */
	@Autowired
	private UserDefinedFieldsLinksRepository userDefinedFieldsLinksRepository;

	/** The loan service. */
	@Autowired
	private LoanService loanService;

	/** The customer service. */
	@Autowired
	private CustomerService customerService;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The Environment. */
	@Autowired
	private Environment environment;

	/** The user Client client. */
	@Autowired
	private UserClient userClient;

	/** The entity manager. */
	@Autowired
	private EntityManager entityManager;
	/** The parametrage client. */
	@Autowired
	private ParametrageClient parametrageClient;

	/** The transvers client. */
	@Autowired
	private TransversClient transversClient;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserDefinedFieldsLinksService#find(java.lang.Long)
	 */
	@Override
	public UserDefinedFieldsLinksDTO find(Long id) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find UserDefinedFieldsLinks by ID : {}", id);
		UserDefinedFieldsLinks userDefinedFieldsLinks =
				userDefinedFieldsLinksRepository.findById(id).orElse(null);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(userDefinedFieldsLinks)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					UserDefinedFieldsLinks.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty("exception.message.not.found")
							+ UserDefinedFieldsLinks.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		return mapper.map(userDefinedFieldsLinks, UserDefinedFieldsLinksDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserDefinedFieldsLinksService#find(com.acm.utils.dtos.
	 * UserDefinedFieldsLinksDTO)
	 */
	@Override
	public List<UserDefinedFieldsLinksDTO> find(
			UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO) {

		Preconditions.checkNotNull(userDefinedFieldsLinksDTO,
				CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		if (ACMValidationUtils.isNullOrEmpty(userDefinedFieldsLinksDTO.getElementId())
				&& ACMValidationUtils.isNullOrEmpty(userDefinedFieldsLinksDTO.getCategory())
				&& ACMValidationUtils.isNullOrEmpty(userDefinedFieldsLinksDTO.getIdAbacusUDFLink())
				&& ACMValidationUtils.isNullOrEmpty(userDefinedFieldsLinksDTO.getUdfGroupIds())
				&& ACMValidationUtils.isNullOrEmpty(userDefinedFieldsLinksDTO.getSurveysId())
				&& ACMValidationUtils
						.isNullOrEmpty(userDefinedFieldsLinksDTO.getUserDefinedFieldsDTO())
				&& ACMValidationUtils.isNullOrEmpty(userDefinedFieldsLinksDTO.getFieldValue())) {
			return new ArrayList<>();

		}
		// init QUserDefinedFieldsLinks
		QUserDefinedFieldsLinks qUserDefinedFieldsLinks =
				QUserDefinedFieldsLinks.userDefinedFieldsLinks;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find only enabled data
		predicate.and(qUserDefinedFieldsLinks.enabled.eq(Boolean.TRUE));

		// // find by customerId
		// if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldsLinksDTO.getCustomerId())) {
		// predicate.and(qUserDefinedFieldsLinks.customerId
		// .eq(userDefinedFieldsLinksDTO.getCustomerId()));
		// }

		// find by elementId
		if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldsLinksDTO.getElementId())) {
			predicate.and(
					qUserDefinedFieldsLinks.elementId.eq(userDefinedFieldsLinksDTO.getElementId()));
		}
		// find by category
		if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldsLinksDTO.getCategory())) {
			predicate.and(
					qUserDefinedFieldsLinks.category.eq(userDefinedFieldsLinksDTO.getCategory()));
		}
		// find by idAbacusUDFLink
		if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldsLinksDTO.getIdAbacusUDFLink())) {
			predicate.and(qUserDefinedFieldsLinks.idAbacusUDFLink
					.eq(userDefinedFieldsLinksDTO.getIdAbacusUDFLink()));
		}
		// find by id udf groups
		if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldsLinksDTO.getUdfGroupIds())) {
			predicate.and(qUserDefinedFieldsLinks.userDefinedFields.userDefinedFieldGroup.id
					.in(userDefinedFieldsLinksDTO.getUdfGroupIds()));
		}
		// // find by loanId
		// if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldsLinksDTO.getLoanId())) {
		// predicate.and(qUserDefinedFieldsLinks.loanId.eq(userDefinedFieldsLinksDTO.getLoanId()));
		// }

		// find by surveysId
		if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldsLinksDTO.getSurveysId())) {
			predicate.and(
					qUserDefinedFieldsLinks.surveysId.eq(userDefinedFieldsLinksDTO.getSurveysId()));
		}

		// find UDFlink by userDefinedField ID and userDefinedFieldGroup ID is NULL
		if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldsLinksDTO.getUserDefinedFieldsDTO())
				&& !ACMValidationUtils
						.isNullOrEmpty(userDefinedFieldsLinksDTO.getUserDefinedFieldsDTO().getId())
				&& ACMValidationUtils.isNullOrEmpty(userDefinedFieldsLinksDTO
						.getUserDefinedFieldsDTO().getUserDefinedFieldGroupDTO())) {
			predicate.and(qUserDefinedFieldsLinks.userDefinedFields.eq(mapper.map(
					userDefinedFieldsLinksDTO.getUserDefinedFieldsDTO(), UserDefinedFields.class)));
		}

		// find UDFlink by userDefinedFieldGroup Code
		if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldsLinksDTO.getUserDefinedFieldsDTO())
				&& !ACMValidationUtils.isNullOrEmpty(userDefinedFieldsLinksDTO
						.getUserDefinedFieldsDTO().getUserDefinedFieldGroupDTO())
				&& !ACMValidationUtils.isNullOrEmpty(userDefinedFieldsLinksDTO
						.getUserDefinedFieldsDTO().getUserDefinedFieldGroupDTO().getCode())) {
			// load list UserDefinedFields by Code udfGroup
			List<UserDefinedFieldsDTO> userDefinedFieldsDTOs =
					parametrageClient.find(userDefinedFieldsLinksDTO.getUserDefinedFieldsDTO());
			List<UserDefinedFields> userDefinedFields = new ArrayList<>();
			userDefinedFieldsDTOs.forEach(userDefinedFieldsDTO -> userDefinedFields
					.add(mapper.map(userDefinedFieldsDTO, UserDefinedFields.class)));
			// find link by userDefinedFields IDs
			predicate.and(qUserDefinedFieldsLinks.userDefinedFields.in(userDefinedFields));
		}

		// find by value : very important for check National id uniqueness
		if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldsLinksDTO.getFieldValue())) {
			predicate.and(qUserDefinedFieldsLinks.fieldValue
					.eq(userDefinedFieldsLinksDTO.getFieldValue()));
		}
		Iterable<UserDefinedFieldsLinks> iterable = userDefinedFieldsLinksRepository
				.findAll(predicate, Sort.by(Direction.ASC, "userDefinedFields"));
		List<UserDefinedFieldsLinks> userDefinedFieldsLinkss = new ArrayList<>();
		iterable.forEach(userDefinedFieldsLinkss::add);
		logger.info("{} : UserDefinedFieldsLinks was founded", userDefinedFieldsLinkss.size());

		List<UserDefinedFieldsLinksDTO> userDefinedFieldsLinkssDTOs = new ArrayList<>();
		userDefinedFieldsLinkss.forEach(userDefinedFieldsLinks -> userDefinedFieldsLinkssDTOs
				.add(mapper.map(userDefinedFieldsLinks, UserDefinedFieldsLinksDTO.class)));
		return userDefinedFieldsLinkssDTOs;

	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserDefinedFieldsLinksService#save(com.acm.utils.dtos.
	 * UserDefinedFieldsLinksDTO)
	 */
	@Override
	public UserDefinedFieldsLinksDTO save(UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO) {

		Preconditions.checkNotNull(userDefinedFieldsLinksDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		UserDefinedFieldsLinks userDefinedFieldsLinks =
				mapper.map(userDefinedFieldsLinksDTO, UserDefinedFieldsLinks.class);

		// setting Default ABACUS UDF LIST VALUE ID if Null
		if (ACMValidationUtils.isNullOrEmpty(userDefinedFieldsLinks.getUdfListValueId())) {
			userDefinedFieldsLinks.setUdfListValueId(0L);
		}
		CommonFunctions.mapperToSave(userDefinedFieldsLinks, userClient, logger);
		UserDefinedFieldsLinks newUserDefinedFieldsLinks =
				userDefinedFieldsLinksRepository.save(userDefinedFieldsLinks);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE,
				UserDefinedFieldsLinks.class.getSimpleName());
		return mapper.map(newUserDefinedFieldsLinks, UserDefinedFieldsLinksDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserDefinedFieldsLinksService#save(java.lang.Long,
	 * com.acm.utils.dtos.UserDefinedFieldsLinksDTO)
	 */
	@Override
	public UserDefinedFieldsLinksDTO save(Long id,
			UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(userDefinedFieldsLinksDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Update UserDefinedFieldsLinks with ID = {}", id);
		UserDefinedFieldsLinks oldUserDefinedFieldsLinks =
				userDefinedFieldsLinksRepository.findById(id).orElse(null);

		// check if object is null
		if (oldUserDefinedFieldsLinks == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					UserDefinedFieldsLinks.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + UserDefinedFieldsLinks.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		// mapping new data with existing data (oldUserDefinedFieldsLinks)
		mapper.map(userDefinedFieldsLinksDTO, oldUserDefinedFieldsLinks);
		// set enabled to FALSE if delete CARD MEZA UDF from ABACUS
		if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldsLinksDTO.getEnableData())) {
			oldUserDefinedFieldsLinks.setEnabled(userDefinedFieldsLinksDTO.getEnableData());
		}
		CommonFunctions.mapperToUpdate(oldUserDefinedFieldsLinks, userClient, logger);

		// setting Default ABACUS UDF LIST VALUE ID if Null
		if (ACMValidationUtils.isNullOrEmpty(oldUserDefinedFieldsLinks.getUdfListValueId())) {
			oldUserDefinedFieldsLinks.setUdfListValueId(0L);
		}

		// update & persist data in DB
		UserDefinedFieldsLinks newUserDefinedFieldsLinks =
				userDefinedFieldsLinksRepository.save(oldUserDefinedFieldsLinks);

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE,
				UserDefinedFieldsLinks.class.getSimpleName());
		return mapper.map(newUserDefinedFieldsLinks, UserDefinedFieldsLinksDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserDefinedFieldsLinkService#saveAll(com.acm.utils.dtos.
	 * userDefinedFieldsLinksDTOs)
	 */
	@Override
	public List<UserDefinedFieldsLinksDTO> saveAll(
			List<UserDefinedFieldsLinksDTO> userDefinedFieldsLinksDTOs) {

		List<UserDefinedFieldsLinksDTO> insertedUserDefinedFieldsLinksDTOs = new ArrayList<>();
		if (!userDefinedFieldsLinksDTOs.isEmpty()) {
			userDefinedFieldsLinksDTOs
					.forEach(userDefinedFieldsLinksDTO -> insertedUserDefinedFieldsLinksDTOs
							.add(save(userDefinedFieldsLinksDTO)));
			logger.info(" {} UDF was inserted.", insertedUserDefinedFieldsLinksDTOs.size());
		}
		return insertedUserDefinedFieldsLinksDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserDefinedFieldsLinksService#saveForTopup(com.acm.utils.dtos.
	 * UserDefinedFieldsLinksDTO)
	 */
	@Override
	public UserDefinedFieldsLinksDTO saveForTopup(
			UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(userDefinedFieldsLinksDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// init check params
		Boolean failed = Boolean.FALSE;
		// get loan from acm db
		LoanDTO loanDTO = loanService.find(userDefinedFieldsLinksDTO.getLoanId());

		if (!ACMValidationUtils.isNullOrEmpty(loanDTO)) {
			userDefinedFieldsLinksDTO.setLoanId(loanDTO.getLoanId());
		}
		else {
			logger.warn(" No LOAN was founeded in ACM DB by acm loan id = {}", loanDTO.getLoanId());
		}
		// check user field object
		if (ACMValidationUtils
				.isNullOrEmpty(userDefinedFieldsLinksDTO.getUserDefinedFieldsDTO().getId())) {
			failed = Boolean.TRUE;
		}
		// check if all data is OK
		if (Boolean.TRUE.equals(failed)) {
			logger.warn(
					"### Failed while saving USER_DEFINED_FIELDS_LINKS in ACM by the given id Abacus UDF Link = {} ###",
					userDefinedFieldsLinksDTO.getIdAbacusUDFLink());
			return userDefinedFieldsLinksDTO;
		}
		// Find UDF link by survey ID to link index group for the same UDF Groups
		if (userDefinedFieldsLinksDTO.getIndexGroup() == null) {
			List<UserDefinedFieldsLinks> definedFieldsLinks = userDefinedFieldsLinksRepository
					.findBySurveysId(userDefinedFieldsLinksDTO.getSurveysId());
			if (!ACMValidationUtils.isNullOrEmpty(definedFieldsLinks)) {
				userDefinedFieldsLinksDTO.setIndexGroup(definedFieldsLinks.get(0).getIndexGroup());
			}
			else {
				userDefinedFieldsLinksDTO.setIndexGroup(userDefinedFieldsLinksDTO.getSurveysId());
			}
		}

		// save in DB
		UserDefinedFieldsLinksDTO newUserDefinedFieldsLinksDTO = save(userDefinedFieldsLinksDTO);
		logger.info(" UserDefinedFieldsLinks was SUCCESFULL CREATED : {}.",
				newUserDefinedFieldsLinksDTO);
		return newUserDefinedFieldsLinksDTO;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserDefinedFieldsLinksService#saveByBatch(com.acm.utils.dtos.
	 * UserDefinedFieldsLinksDTO)
	 */
	@Override
	public UserDefinedFieldsLinksDTO saveByBatch(
			UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(userDefinedFieldsLinksDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// init check params
		Boolean failed = Boolean.FALSE;
		// check if UDF exit in ACM by ID UDF link ABACUS
		if (userDefinedFieldsLinksDTO.getCuAccountId() != null) {
			// find loan by cuAccount
			LoanDTO loanDTO =
					loanService.findByIdAccountExtern(userDefinedFieldsLinksDTO.getCuAccountId());
			if (!ACMValidationUtils.isNullOrEmpty(loanDTO)) {
				// setting id loan
				userDefinedFieldsLinksDTO.setElementId(loanDTO.getLoanId());
				userDefinedFieldsLinksDTO.setCategory(CommonConstants.LOAN_CATEGORY);
			}
			else {
				logger.info(" No LOAN was founeded in ACM DB by ID_ACCOUNT_EXTERN = {}",
						userDefinedFieldsLinksDTO.getCuAccountId());
				failed = Boolean.TRUE;
			}
		}
		else if (userDefinedFieldsLinksDTO.getCustomerId() != null) {
			// find customer by Customer Id ABACUS
			List<CustomerDTO> customerDTOs = customerService
					.findCustomerIdExtern(userDefinedFieldsLinksDTO.getCustomerId(), null);
			if (!ACMValidationUtils.isNullOrEmpty(customerDTOs)) {
				// setting customer id ACM
				userDefinedFieldsLinksDTO.setCustomerId(customerDTOs.get(0).getId());
				userDefinedFieldsLinksDTO.setElementId(customerDTOs.get(0).getId());
				userDefinedFieldsLinksDTO.setCategory(CommonConstants.CUSTOMER_CATEGORY);
			}
			else {
				logger.info("No CUSTOMER was founeded in ACM DB by ID_CUSTOMER_EXTERN = {}",
						userDefinedFieldsLinksDTO.getCustomerId());
				failed = Boolean.TRUE;
			}
		}
		// check user field object
		if (ACMValidationUtils
				.isNullOrEmpty(userDefinedFieldsLinksDTO.getUserDefinedFieldsDTO().getId())) {
			failed = Boolean.TRUE;
		}
		// check if all data is OK
		if (Boolean.TRUE.equals(failed)) {
			logger.info(
					"### Failed while saving USER_DEFINED_FIELDS_LINKS in ACM by the given id Abacus UDF Link = {} ###",
					userDefinedFieldsLinksDTO.getIdAbacusUDFLink());
			return userDefinedFieldsLinksDTO;
		}
		// Find UDF link by survey ID to link index group for the same UDF Groups TODO
		userDefinedFieldsLinksDTO.setIndexGroup(userDefinedFieldsLinksDTO.getSurveysId());
		// save in DB
		UserDefinedFieldsLinksDTO newUserDefinedFieldsLinksDTO = save(userDefinedFieldsLinksDTO);
		logger.info(" UserDefinedFieldsLinks was SUCCESFULL CREATED : {}.",
				newUserDefinedFieldsLinksDTO);
		return newUserDefinedFieldsLinksDTO;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserDefinedFieldsLinkService#deleteAllByCustomer(com.acm.utils.dtos.
	 * userDefinedFieldsLinksDTOs)
	 */
	@Override
	public void deleteAllByCustomer(Long customerId) {

		Preconditions.checkNotNull(customerId, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info(
				"Delete all userDefinedFieldsLinks by Customer by ID : {} And IdAbacusUDFLink is NULL",
				customerId);
		userDefinedFieldsLinksRepository.deleteByElementIdAndCategory(customerId,
				CommonConstants.CUSTOMER_CATEGORY);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserDefinedFieldsLinksService#deleteAllByCategory(java.lang.Long,
	 * java.lang.String)
	 */
	@Override
	public void deleteAllByCategory(Long elementId, String category) {

		Preconditions.checkNotNull(elementId, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info(
				"Delete all userDefinedFieldsLinks by category:{} and by elementID : {} And IdAbacusUDFLink is NULL",
				category, elementId);
		userDefinedFieldsLinksRepository.deleteByElementIdAndCategory(elementId, category);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserDefinedFieldsLinkService#deleteAllByLoan(com.acm.utils.dtos.
	 * userDefinedFieldsLinksDTOs)
	 */
	@Override
	public void deleteAllByLoan(Long loanId) {

		Preconditions.checkNotNull(loanId, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Delete all userDefinedFieldsLinks by loan by ID : {} ", loanId);
		userDefinedFieldsLinksRepository.deleteByElementIdAndCategory(loanId,
				CommonConstants.LOAN_CATEGORY);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserDefinedFieldsLinksService#findUDFGroupBy(com.acm.utils.dtos.
	 * UserDefinedFieldsLinksDTO)
	 */
	@Override
	public List<UDFLinksGroupeFieldsDTO> findUDFGroupBy(
			UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO) {

		// find list UDF link
		List<UserDefinedFieldsLinksDTO> userDefinedFieldsLinksDTOs =
				find(userDefinedFieldsLinksDTO);

		userDefinedFieldsLinksDTOs = userDefinedFieldsLinksDTOs.stream()
				.sorted(Comparator
						.comparingLong(udf -> udf.getUserDefinedFieldsDTO().getOrdre() != null
								? udf.getUserDefinedFieldsDTO().getOrdre()
								: udf.getUserDefinedFieldsDTO().getId()))
				.collect(Collectors.toList());

		// get list of GROUP GROUP INDEX
		List<Long> groupIDindex = new ArrayList<>();
		for (UserDefinedFieldsLinksDTO dto : userDefinedFieldsLinksDTOs) {
			groupIDindex.add(dto.getIndexGroup());
		}
		// filter list group ID (Remove Duplicates)
		List<Long> groupIDindexWithoutDuplicates = new ArrayList<>(new HashSet<>(groupIDindex));

		// get list of GROUP ID ABACUS
		Map<Long, Long> groupIDList = new HashMap<>();
		for (UserDefinedFieldsLinksDTO dto : userDefinedFieldsLinksDTOs) {
			if (groupIDindexWithoutDuplicates.indexOf(dto.getIndexGroup()) != -1) {
				groupIDList.put(dto.getIndexGroup(),
						dto.getUserDefinedFieldsDTO().getUserDefinedFieldGroupDTO().getId());
				groupIDindexWithoutDuplicates
						.remove(groupIDindexWithoutDuplicates.indexOf(dto.getIndexGroup()));
			}
		}

		List<UDFLinksGroupeFieldsDTO> udfGroupeFields = new ArrayList<>();
		for (Map.Entry<Long, Long> mapGroupIndex : groupIDList.entrySet()) {
			Long idUDFGroup = mapGroupIndex.getValue();
			Long indexGroup = mapGroupIndex.getKey();
			// init object
			UDFLinksGroupeFieldsDTO udfGroupeField = new UDFLinksGroupeFieldsDTO();
			udfGroupeField.setUserDefinedFieldGroupID(idUDFGroup);

			List<UDFLinksGroupeFieldsModelDTO> udfGroupeFieldsModels = new ArrayList<>();
			for (UserDefinedFieldsLinksDTO dto : userDefinedFieldsLinksDTOs) {
				if (dto.getUserDefinedFieldsDTO().getUserDefinedFieldGroupDTO().getId()
						.equals(idUDFGroup) && dto.getIndexGroup().equals(indexGroup)) {
					// set name of the value selected from udf field list values
					if ((!ACMValidationUtils.isNullOrEmpty(dto.getUdfListValueId())
							&& dto.getUdfListValueId() != 0)
							&& (!ACMValidationUtils.isNullOrEmpty(dto.getFieldValue()))
							&& NumberUtils.isNumeric(dto.getFieldValue())) {

						// init UDF list value params
						UserDefinedFieldListValuesDTO params = new UserDefinedFieldListValuesDTO(
								dto.getUdfListValueId(), Long.parseLong(dto.getFieldValue()));
						// find by [ID_UDF_LIST_VALUE]
						List<UserDefinedFieldListValuesDTO> userDefinedFieldListValuesDTOs =
								parametrageClient
										.findByIdUDFListValue(Long.parseLong(dto.getFieldValue()));
						// setting id parent if exist
						if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldListValuesDTOs)) {
							params.setParentUDFListValue(
									userDefinedFieldListValuesDTOs.get(0).getParentUDFListValue());
						}
						List<UserDefinedFieldListValuesDTO> listValuesDTOs =
								parametrageClient.find(params);
						dto.setFieldValueId(Long.parseLong(dto.getFieldValue()));
						if (!listValuesDTOs.isEmpty()) {
							dto.setFieldValue(listValuesDTOs.get(0).getDescription());
						}
					}
					// setting group name
					udfGroupeField.setUserDefinedFieldGroupName(
							dto.getUserDefinedFieldsDTO().getUserDefinedFieldGroupDTO().getCode());
					// setting mandatory
					udfGroupeField.setMondatory(dto.getUserDefinedFieldsDTO()
							.getUserDefinedFieldGroupDTO().getMondatory());

					UDFLinksGroupeFieldsModelDTO udfGroupeFieldsModel =
							new UDFLinksGroupeFieldsModelDTO(dto.getUserDefinedFieldsDTO().getId(),
									dto.getFieldValue(), dto.getUdfListValueId(), 1,
									dto.getUserDefinedFieldsDTO().getFieldMasc(),
									dto.getUserDefinedFieldsDTO().getMandatory(),
									dto.getUserDefinedFieldsDTO().getUniqueField(), Boolean.FALSE,
									dto.getUserDefinedFieldsDTO().getFieldType(),
									dto.getUserDefinedFieldsDTO().getName(), "1", null,
									dto.getUserDefinedFieldsDTO().getUserDefinedFieldGroupDTO()
											.getCustomerType());
					// setting IDs
					udfGroupeFieldsModel.setId(dto.getId());
					udfGroupeFieldsModel.setSurveysId(dto.getSurveysId());
					udfGroupeFieldsModel.setIndexGroup(dto.getIndexGroup());
					udfGroupeFieldsModel.setIdAbacusUDFLink(dto.getIdAbacusUDFLink());
					udfGroupeFieldsModel.setFieldValueId(dto.getFieldValueId());
					udfGroupeFieldsModels.add(udfGroupeFieldsModel);
				}
			}
			udfGroupeField.setUdfGroupeFieldsModels(udfGroupeFieldsModels);
			udfGroupeFields.add(udfGroupeField);
		}

		// Group mandatory = TRUE && not in groupIDWithoutDuplicates
		UserDefinedFieldGroupDTO params = new UserDefinedFieldGroupDTO();
		if (userDefinedFieldsLinksDTO.getCategory().equals(CommonConstants.LOAN_CATEGORY)) {
			// FIND ONLY UDF Group for LOAN
			params.setCategory(CommonConstants.LOAN_CATEGORY);
			params.setProductId(userDefinedFieldsLinksDTO.getProductId());
		}
		else if (userDefinedFieldsLinksDTO.getCategory()
				.equals(CommonConstants.CUSTOMER_CATEGORY)) {
			// FIND ONLY UDF Group for CUSTOMER
			params.setCategory(CommonConstants.CUSTOMER_CATEGORY);
			params.setCustomerTypeLabel(userDefinedFieldsLinksDTO.getCutomerType());
		}
		else if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldsLinksDTO.getCategory())) {
			// FIND ONLY UDF Group for COLLECTION / SUPPLIER / THIRD PARTY
			params.setCategory(userDefinedFieldsLinksDTO.getCategory());
		}
		List<UserDefinedFieldGroupDTO> userDefinedFieldGroupDTOs = parametrageClient.find(params);
		for (UserDefinedFieldGroupDTO dto : userDefinedFieldGroupDTOs) {
			if (!(groupIDList.containsValue(dto.getId()))
					&& Boolean.TRUE.equals(dto.getMondatory())) {
				// init object
				UDFLinksGroupeFieldsDTO udfGroupeField = new UDFLinksGroupeFieldsDTO();
				udfGroupeField.setUdfGroupeFieldsModels(new ArrayList<>());
				udfGroupeField.setUserDefinedFieldGroupID(dto.getId());
				udfGroupeField.setUserDefinedFieldGroupName(dto.getCode());
				udfGroupeField.setMondatory(dto.getMondatory());
				udfGroupeFields.add(udfGroupeField);
			}
		}
		return udfGroupeFields;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserDefinedFieldsLinksService#delete(com.acm.utils.dtos.
	 * UserDefinedFieldsLinksDTO)
	 */
	@Override
	public void delete(UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO) {

		Preconditions.checkNotNull(userDefinedFieldsLinksDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Preconditions.checkNotNull(userDefinedFieldsLinksDTO.getId(),
				CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.warn("delete UserDefinedFieldsLinks  with ID = {}",
				userDefinedFieldsLinksDTO.getId());
		// delete object by id
		userDefinedFieldsLinksRepository.deleteById(userDefinedFieldsLinksDTO.getId());
		logger.info(CommonLoggerMessage.SUCCESFULL_DELETE,
				UserDefinedFieldsLinksDTO.class.getSimpleName());
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserDefinedFieldsLinksService#findUDFLoansGroupBy(com.acm.utils.dtos.
	 * UserDefinedFieldsLinksDTO)
	 */
	@Override
	public List<LoansUdfDTO> findUDFLoansGroupBy(
			UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO) {

		Preconditions.checkNotNull(userDefinedFieldsLinksDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		if (ACMValidationUtils.isNullOrEmpty(userDefinedFieldsLinksDTO.getElementId())) {
			return new ArrayList<>();
		}
		List<LoansUdfDTO> loansUdfDTOs = new ArrayList<>();
		// List loans by Customer ID
		List<LoanDTO> loanDTOs =
				loanService.findByIdCustomer(userDefinedFieldsLinksDTO.getElementId());

		// List udf by loans ID
		for (LoanDTO loanDTO : loanDTOs) {
			// find udf by given ID loan
			UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTOParams =
					new UserDefinedFieldsLinksDTO();
			userDefinedFieldsLinksDTOParams.setProductId(loanDTO.getProductId().toString());
			userDefinedFieldsLinksDTOParams.setCategory(CommonConstants.LOAN_CATEGORY);
			userDefinedFieldsLinksDTOParams.setElementId(loanDTO.getLoanId());
			List<UDFLinksGroupeFieldsDTO> acmLoansUdfss =
					findUDFGroupBy(userDefinedFieldsLinksDTOParams);

			logger.info("{} : Udf was founded by ID loan = {}", acmLoansUdfss.size(),
					loanDTO.getLoanId());
			// mapping returned list
			List<UDFLinksGroupeFieldsDTO> acmUdfLinksGroupeFieldDTOs = new ArrayList<>();
			acmLoansUdfss.forEach(acmLoansUdfs -> acmUdfLinksGroupeFieldDTOs
					.add(mapper.map(acmLoansUdfs, UDFLinksGroupeFieldsDTO.class)));
			// init object
			loansUdfDTOs.add(new LoansUdfDTO(loanDTO.getLoanId(), loanDTO.getAccountNumber(),
					acmUdfLinksGroupeFieldDTOs));
		}
		return loansUdfDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserDefinedFieldsLinksService#
	 * deleteByCustomerIdAndIdAbacusUDFLinkIsNullAndSurveysIdIsNull(java.lang.Long)
	 */
	@Override
	public void deleteByCustomerIdAndIdAbacusUDFLinkIsNullAndSurveysIdIsNull(Long customerId) {

		Preconditions.checkNotNull(customerId, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.warn("deleteByCustomerIdAndIdAbacusUDFLinkIsNullAndSurveysIdIsNull  with ID = {}",
				customerId);
		// delete object by id
		userDefinedFieldsLinksRepository
				.deleteByElementIdAndCategoryAndIdAbacusUDFLinkIsNullAndSurveysIdIsNull(customerId,
						CommonConstants.CUSTOMER_CATEGORY);
		logger.info(CommonLoggerMessage.SUCCESFULL_DELETE,
				UserDefinedFieldsLinksDTO.class.getSimpleName());

	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserDefinedFieldsLinksService#
	 * deleteByLoanIdAndIdAbacusUDFLinkIsNullAndSurveysIdIsNull(java.lang.Long)
	 */
	@Override
	public void deleteByLoanIdAndIdAbacusUDFLinkIsNullAndSurveysIdIsNull(Long loanId) {

		Preconditions.checkNotNull(loanId, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.warn("deleteByLoanIdAndIdAbacusUDFLinkIsNullAndSurveysIdIsNull with ID = {}",
				loanId);
		// delete object by id
		userDefinedFieldsLinksRepository
				.deleteByElementIdAndCategoryAndIdAbacusUDFLinkIsNullAndSurveysIdIsNull(loanId,
						CommonConstants.LOAN_CATEGORY);
		logger.info(CommonLoggerMessage.SUCCESFULL_DELETE,
				UserDefinedFieldsLinksDTO.class.getSimpleName());
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserDefinedFieldsLinksService#updateUdfLinksByElementId(java.util.List)
	 */
	@Override
	public void updateAcmUdfLinksByElementId(
			List<UserDefinedFieldsLinksDTO> userDefinedFieldsLinksDTOs, Long elementId,
			String category) throws ResourcesNotFoundException {

		// Update UDF
		if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldsLinksDTOs)) {
			for (UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO : userDefinedFieldsLinksDTOs) {
				userDefinedFieldsLinksDTO.setElementId(elementId);
				if (ACMValidationUtils.isNullOrEmpty(userDefinedFieldsLinksDTO.getCategory())) {
					userDefinedFieldsLinksDTO.setCategory(category);
				}
				// check if is not null
				if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldsLinksDTO.getId())
						&& !ACMValidationUtils
								.isNullOrEmpty(userDefinedFieldsLinksDTO.getFieldValue())) {
					// Update data In ACM DB
					save(userDefinedFieldsLinksDTO.getId(), userDefinedFieldsLinksDTO);
				}
				else if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldsLinksDTO.getId())
						&& ACMValidationUtils
								.isNullOrEmpty(userDefinedFieldsLinksDTO.getFieldValue())) {

					// delete UDF from ACM DB by ID
					delete(userDefinedFieldsLinksDTO);
				}
				else if (ACMValidationUtils.isNullOrEmpty(userDefinedFieldsLinksDTO.getId())
						&& !ACMValidationUtils
								.isNullOrEmpty(userDefinedFieldsLinksDTO.getFieldValue())) {

					// save data in ACM
					save(userDefinedFieldsLinksDTO);
				}
			}
		}

	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.UserDefinedFieldsLinksService#updateAbacusUdfLinksByElementId(java.util.List,
	 * java.lang.Long, java.lang.String, java.lang.Long, java.lang.Object)
	 */
	@Override
	public void updateAbacusUdfLinksByElementId(Long elementId, String category, Long idExtern,
			Object object) throws ResourcesNotFoundException {

		String loanApplicationStatus = "";
		logger.info("saving and updating udf");
		// access to attribute values of the object
		if (!ACMValidationUtils.isNullOrEmpty(object)) {
			Class<?> objectClass = object.getClass();
			Field loanApplicationStatusField;
			try {
				loanApplicationStatusField = objectClass.getDeclaredField("loanApplicationStatus");
				// Make the attribute accessible (if it's private, protected, or package-private)
				loanApplicationStatusField.setAccessible(true);

				// Get the value of the attribute from the object
				loanApplicationStatus = loanApplicationStatusField.get(object).toString();
			}
			catch (NoSuchFieldException | SecurityException | IllegalArgumentException
					| IllegalAccessException e) {
				e.printStackTrace();
			}
		}

		List<UserDefinedFieldsLinksDTO> listUDFLinks = new ArrayList<>();
		switch (category) {
			case CommonConstants.LOAN_CATEGORY:
				listUDFLinks = transversClient.loadUDFByLoan(idExtern != null ? idExtern : 0L);
				break;

			case CommonConstants.CUSTOMER_CATEGORY:
				// loading list UDF customer from ABACUS DB
				listUDFLinks = transversClient.loadUDFByCustomer(idExtern);
				break;

			case CommonConstants.COLLATERAL_CATEGORY:
				break;

		}

		logger.info("List UDFs Links size = {}", listUDFLinks.size());
		if (!ACMValidationUtils.isNullOrEmpty(listUDFLinks)) {
			ArrayList<UserDefinedFieldsDTO> listUDFFields = new ArrayList<>();
			listUDFLinks.forEach(udfLink -> listUDFFields.add(udfLink.getUserDefinedFieldsDTO()));
			logger.info("List UDFs customer size = {}", listUDFFields.size());
			// find && setting udf field object
			List<UserDefinedFieldsDTO> userDefinedFieldsDTOs =
					parametrageClient.findUDFFieldByListIds(listUDFFields);
			logger.info("List UDFs customer size = {}", userDefinedFieldsDTOs.size());
			// delete all udfLinks by idUDFField(udf field id in ABACUS) from ACM_UDF_LINKS : case
			// edit
			UserDefinedFieldsLinksDTO param = new UserDefinedFieldsLinksDTO();
			param.setElementId(elementId);
			param.setCategory(category);
			List<Long> idUDFFieldList =
					userDefinedFieldsDTOs.stream().map(UserDefinedFieldsDTO::getIdUDFField)
							.distinct().collect(Collectors.toList());
			userDefinedFieldsLinksRepository.deleteByElementIdAndCategoryAndUserDefinedFieldsIn(
					elementId, category, idUDFFieldList);
			// save udfLinks from ABACUS IN ACM
			for (UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO : listUDFLinks) {
				userDefinedFieldsLinksDTO.setCategory(category);
				userDefinedFieldsLinksDTO.setElementId(elementId);
				for (UserDefinedFieldsDTO udfDTO : userDefinedFieldsDTOs) {
					if (userDefinedFieldsLinksDTO.getUserDefinedFieldsDTO().getIdUDFField()
							.equals(udfDTO.getIdUDFField())) {
						userDefinedFieldsLinksDTO.setUserDefinedFieldsDTO(udfDTO);
						switch (category) {
							case CommonConstants.LOAN_CATEGORY:
								// if loanDTO is Topup or Refinance and action is 'Save Loan' then
								// create
								// udfLink for loanDTO same as the original loan's udfLink
								if (loanApplicationStatus.equals(CommonConstants.TOPUP)
										|| loanApplicationStatus
												.equals(CommonConstants.REFINANCE)) {
									userDefinedFieldsLinksDTO =
											saveForTopup(userDefinedFieldsLinksDTO);
								}
								else {
									userDefinedFieldsLinksDTO =
											saveByBatch(userDefinedFieldsLinksDTO);
								}
								break;

							case CommonConstants.CUSTOMER_CATEGORY:

								saveByBatch(userDefinedFieldsLinksDTO);
								break;

							case CommonConstants.COLLATERAL_CATEGORY:
								break;

						}
					}
				}
			}

		}

	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserDefinedFieldsLinksService#findMaxIndexGroup(java.lang.Long,
	 * java.lang.String)
	 */
	@Override
	public Integer findMaxIndexGroup(Long elementId, String category) {

		Integer result = null;
		if (!ACMValidationUtils.isNullOrEmpty(elementId)) {
			List<Long> indexGroupList = userDefinedFieldsLinksRepository
					.findIndexGroupByElementIdAndCategoryOrderByIndexGroupDesc(elementId, category);
			result = (!ACMValidationUtils.isNullOrEmpty(indexGroupList))
					? indexGroupList.get(0).intValue()
					: null;
		}
		return result;

	}

}
