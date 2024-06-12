/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Service;

import com.acm.client.TransversClient;
import com.acm.client.UserClient;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.UserDefinedFieldsRepository;
import com.acm.service.UserDefinedFieldGroupService;
import com.acm.service.UserDefinedFieldListValuesService;
import com.acm.service.UserDefinedFieldsService;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.dtos.UserDefinedFieldGroupDTO;
import com.acm.utils.dtos.UserDefinedFieldListValuesDTO;
import com.acm.utils.dtos.UserDefinedFieldsDTO;
import com.acm.utils.enums.SettingUDFFieldsType;
import com.acm.utils.models.QUserDefinedFields;
import com.acm.utils.models.UserDefinedFieldGroup;
import com.acm.utils.models.UserDefinedFields;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link UserDefinedFieldsServiceImpl} Class Impl.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
@Service
public class UserDefinedFieldsServiceImpl implements UserDefinedFieldsService {

	/** The Constant logger. */
	private static final Logger logger =
			LoggerFactory.getLogger(UserDefinedFieldsServiceImpl.class);

	/** The userDefinedFields repository. */
	@Autowired
	private UserDefinedFieldsRepository userDefinedFieldsRepository;

	/** The user defined field group service. */
	@Autowired
	private UserDefinedFieldGroupService userDefinedFieldGroupService;

	/** The user defined field list values service. */
	@Autowired
	private UserDefinedFieldListValuesService userDefinedFieldListValuesService;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The Environment. */
	@Autowired
	private Environment environment;

	/** The user Client client. */
	@Autowired
	private UserClient userClient;

	/** The setting transvers client. */
	@Autowired
	private TransversClient settingTransversClient;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserDefinedFieldsService#find(java.lang.Long)
	 */
	@Override
	public UserDefinedFieldsDTO find(Long id) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find UserDefinedFields by ID : {}", id);
		UserDefinedFields userDefinedFields = userDefinedFieldsRepository.findById(id).orElse(null);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(userDefinedFields)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					UserDefinedFields.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty("exception.message.not.found")
							+ UserDefinedFields.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		return mapper.map(userDefinedFields, UserDefinedFieldsDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserDefinedFieldsService#find(com.acm.utils.dtos.UserDefinedFieldsDTO)
	 */
	@Override
	public List<UserDefinedFieldsDTO> find(UserDefinedFieldsDTO userDefinedFieldsDTO) {

		Preconditions.checkNotNull(userDefinedFieldsDTO,
				CommonExceptionsMessage.EXCEPTIONS_ID_NULL);

		// init QUserDefinedFields
		QUserDefinedFields qUserDefinedFields = QUserDefinedFields.userDefinedFields;

		// init Predicate
		BooleanBuilder predicate = buildQuery(userDefinedFieldsDTO, qUserDefinedFields);

		// finding data from DB
		Iterable<UserDefinedFields> iterable = userDefinedFieldsRepository.findAll(predicate);
		List<UserDefinedFields> userDefinedFieldss = new ArrayList<>();
		iterable.forEach(userDefinedFieldss::add);
		userDefinedFieldss = userDefinedFieldss.stream()
				.sorted(Comparator.comparingLong(
						udf -> udf.getOrdre() != null ? udf.getOrdre() : udf.getId()))
				.collect(Collectors.toList());
		logger.info("{} : UserDefinedFields was founded", userDefinedFieldss.size());

		// mapping data && loading list value if exist
		List<UserDefinedFieldsDTO> userDefinedFieldssDTOs = new ArrayList<>();
		userDefinedFieldss.forEach(userDefinedFields -> {
			UserDefinedFieldsDTO dto = mapper.map(userDefinedFields, UserDefinedFieldsDTO.class);
			if (dto.getFieldType().equals(SettingUDFFieldsType.LIST.typeId())
					&& dto.getIdUDFListValue() != 0) {
				dto.getFieldListValuesDTOs().addAll(userDefinedFieldListValuesService
						.find(new UserDefinedFieldListValuesDTO(dto.getIdUDFListValue(), null)));
			}
			userDefinedFieldssDTOs.add(dto);
		});
		return userDefinedFieldssDTOs;
	}

	/**
	 * Builds the query.
	 *
	 * @author HaythemBenizid
	 * @param userDefinedFieldsDTO the user defined fields DTO
	 * @param qUserDefinedFields the q user defined fields
	 * @return the boolean builder
	 */
	private BooleanBuilder buildQuery(UserDefinedFieldsDTO userDefinedFieldsDTO,
			QUserDefinedFields qUserDefinedFields) {

		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find only enabled data
		predicate.and(qUserDefinedFields.enabled.eq(Boolean.TRUE));

		if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldsDTO.getIdUDFField())) {
			if (userDefinedFieldsDTO.getIdUDFField() == 0L) {
				predicate.and(qUserDefinedFields.idUDFField.eq(0L));
			}
		}
		else {
			// find only enabled data
			predicate.and(qUserDefinedFields.enabled.eq(Boolean.TRUE));
			// predicate.and(qUserDefinedFields.idUDFField.ne(0L));
		}

		// find by idUDFListValue
		if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldsDTO.getIdUDFListValue())) {
			predicate.and(
					qUserDefinedFields.idUDFListValue.eq(userDefinedFieldsDTO.getIdUDFListValue()));
		}

		// find by ID
		if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldsDTO.getId())) {
			predicate.and(qUserDefinedFields.id.eq(userDefinedFieldsDTO.getId()));
		}
		// find by id udf fields in ABACUS
		if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldsDTO.getIdUDFField())) {
			predicate.and(qUserDefinedFields.idUDFField.eq(userDefinedFieldsDTO.getIdUDFField()));
		}

		// find by table Abacus Name
		if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldsDTO.getUserDefinedFieldGroupDTO())
				&& !ACMValidationUtils.isNullOrEmpty(
						userDefinedFieldsDTO.getUserDefinedFieldGroupDTO().getId())) {
			predicate.and(qUserDefinedFields.userDefinedFieldGroup.eq(new UserDefinedFieldGroup(
					userDefinedFieldsDTO.getUserDefinedFieldGroupDTO().getId())));
		}
		// find by table Group Code
		if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldsDTO.getUserDefinedFieldGroupDTO())
				&& !ACMValidationUtils.isNullOrEmpty(
						userDefinedFieldsDTO.getUserDefinedFieldGroupDTO().getCode())) {
			predicate.and(qUserDefinedFields.userDefinedFieldGroup.code
					.eq(userDefinedFieldsDTO.getUserDefinedFieldGroupDTO().getCode()));
		}

		// find by table Group Code
		if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldsDTO.getUserDefinedFieldGroupDTO())
				&& !ACMValidationUtils.isNullOrEmpty(
						userDefinedFieldsDTO.getUserDefinedFieldGroupDTO().getCode())) {
			predicate.and(qUserDefinedFields.userDefinedFieldGroup.code
					.eq(userDefinedFieldsDTO.getUserDefinedFieldGroupDTO().getCode()));
		}

		// find by fields name in
		if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldsDTO.getNames())) {
			predicate.and(qUserDefinedFields.name.in(userDefinedFieldsDTO.getNames()));
		}

		// find by field Type
		if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldsDTO.getFieldType())) {
			predicate.and(qUserDefinedFields.fieldType.eq(userDefinedFieldsDTO.getFieldType()));
		}

		// find by id Abacus UDF Field
		if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldsDTO.getIdUDFField())) {
			predicate.and(qUserDefinedFields.idUDFField.eq(userDefinedFieldsDTO.getIdUDFField()));
		}

		return predicate;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserDefinedFieldsService#save(com.acm.utils.dtos.UserDefinedFieldsDTO)
	 */
	@Override
	public UserDefinedFieldsDTO save(UserDefinedFieldsDTO userDefinedFieldsDTO, UserDTO userDTO) {

		Preconditions.checkNotNull(userDefinedFieldsDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		UserDefinedFields userDefinedFields =
				mapper.map(userDefinedFieldsDTO, UserDefinedFields.class);

		if (ACMValidationUtils.isNullOrEmpty(userDTO.getNom())) {
			userDTO = CommonFunctions.getConnectedUser(logger);
		}

		userDefinedFields.setInsertBy(userDTO.getFullName());
		userDefinedFields.setAcmVersion(0);
		userDefinedFields.setDateInsertion(new Date());
		userDefinedFields.setEnabled(userDefinedFieldsDTO.getEnabled());

		UserDefinedFields newUserDefinedFields =
				userDefinedFieldsRepository.save(userDefinedFields);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, UserDefinedFields.class.getSimpleName());
		return mapper.map(newUserDefinedFields, UserDefinedFieldsDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserDefinedFieldsService#save(java.lang.Long,
	 * com.acm.utils.dtos.UserDefinedFieldsDTO)
	 */
	@Override
	public UserDefinedFieldsDTO save(Long id, UserDefinedFieldsDTO userDefinedFieldsDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(userDefinedFieldsDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.info("Update UserDefinedFields with ID = {}", id);
		UserDefinedFields oldUserDefinedFields =
				userDefinedFieldsRepository.findById(id).orElse(null);

		// check if object is null
		if (oldUserDefinedFields == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					UserDefinedFields.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + UserDefinedFields.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		// mapping new data with existing data (oldUserDefinedFields)
		mapper.map(userDefinedFieldsDTO, oldUserDefinedFields);
		CommonFunctions.mapperToUpdate(oldUserDefinedFields, userClient, logger);

		// update & persist data in DB
		UserDefinedFields newUserDefinedFields =
				userDefinedFieldsRepository.save(oldUserDefinedFields);

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, UserDefinedFields.class.getSimpleName());
		return mapper.map(newUserDefinedFields, UserDefinedFieldsDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserDefinedFieldsService#loadSettingFromAbacus()
	 */
	@Override
	public void loadSettingFromAbacus(UserDTO userDTO) throws ResourcesNotFoundException {

		// load list UDF Fields from ABACUS DB
		List<UserDefinedFieldsDTO> userDefinedFieldListValuesDTOs =
				settingTransversClient.findUserDefinedFields();
		for (UserDefinedFieldsDTO userDefinedFieldsDTO : userDefinedFieldListValuesDTOs) {
			logger.debug("{}", userDefinedFieldsDTO);
			// check if UDF Fields exist in ACM DB
			List<UserDefinedFields> userDefinedFields = userDefinedFieldsRepository
					.findByIdUDFFieldAndEnabled(userDefinedFieldsDTO.getIdUDFField(), Boolean.TRUE);
			// get UDF Group data from ACM DB by id abacus group
			List<UserDefinedFieldGroupDTO> userDefinedFieldGroupDTOs = userDefinedFieldGroupService
					.find(new UserDefinedFieldGroupDTO(userDefinedFieldsDTO
							.getUserDefinedFieldGroupDTO().getIdUDGroupAbacus()));
			// save or update returned data
			if (ACMValidationUtils.isNullOrEmpty(userDefinedFields)
					&& !ACMValidationUtils.isNullOrEmpty(userDefinedFieldGroupDTOs)) {
				// check if field is mondatory and its group is not mondatory
				if (Boolean.TRUE.equals(userDefinedFieldsDTO.getMandatory())
						&& Boolean.FALSE.equals(userDefinedFieldGroupDTOs.get(0).getMondatory())) {
					userDefinedFieldGroupDTOs.get(0).setMondatory(Boolean.TRUE);
					userDefinedFieldGroupService.save(userDefinedFieldGroupDTOs.get(0).getId(),
							userDefinedFieldGroupDTOs.get(0));
				}
				// Save new entry
				userDefinedFieldsDTO.setUserDefinedFieldGroupDTO(userDefinedFieldGroupDTOs.get(0));
				save(userDefinedFieldsDTO, userDTO);
			}
			else if (!ACMValidationUtils.isNullOrEmpty(userDefinedFields)
					&& !ACMValidationUtils.isNullOrEmpty(userDefinedFieldGroupDTOs)) {
				// check if field is mondatory and its group is not mondatory
				if (Boolean.TRUE.equals(userDefinedFieldsDTO.getMandatory())
						&& Boolean.FALSE.equals(userDefinedFieldGroupDTOs.get(0).getMondatory())) {
					userDefinedFieldGroupDTOs.get(0).setMondatory(Boolean.TRUE);
					userDefinedFieldGroupService.save(userDefinedFieldGroupDTOs.get(0).getId(),
							userDefinedFieldGroupDTOs.get(0));
				}
				// Update existing UDF Group in ACM DB
				if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldGroupDTOs)) {
					logger.debug("{}", userDefinedFields.get(0));
					userDefinedFieldsDTO
							.setUserDefinedFieldGroupDTO(userDefinedFieldGroupDTOs.get(0));
					// setting ID
					userDefinedFieldsDTO.setId(userDefinedFields.get(0).getId());
					UserDefinedFields udfFields =
							mapper.map(userDefinedFieldsDTO, UserDefinedFields.class);
					// mapping new data with existing data (udfFields)
					CommonFunctions.mapperToUpdate(udfFields, userClient, logger);
					// update & persist data in DB
					UserDefinedFields newUserDefinedFields =
							userDefinedFieldsRepository.save(udfFields);
					logger.debug("{}", newUserDefinedFields);
					logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE,
							UserDefinedFields.class.getSimpleName());
				}
			}
		}

		// udf from abacus

		// udf from acm
		List<UserDefinedFields> userDefinedFields = userDefinedFieldsRepository.findAll();
		List<UserDefinedFieldsDTO> userDefinedFieldsDTO = new ArrayList<UserDefinedFieldsDTO>();

		userDefinedFields.forEach(item -> {
			userDefinedFieldsDTO.add(mapper.map(item, UserDefinedFieldsDTO.class));

		});
		List<Long> lstIdUIAbacus = new ArrayList<Long>();
		userDefinedFieldListValuesDTOs.forEach(item -> {
			lstIdUIAbacus.add(item.getIdUDFField());

		});
		userDefinedFieldsDTO.forEach(item -> {
			// check if IdUDFField is not not null (check that item is Abacus udf field and not Acm
			// udf Field)
			if (!ACMValidationUtils.isNullOrEmpty(item.getIdUDFField())
					&& !lstIdUIAbacus.contains(item.getIdUDFField())) {
				item.setEnabled(false);
				userDefinedFieldsRepository.save(mapper.map(item, UserDefinedFields.class));
			}
		});

		checkIfAllFieldsNotMandatoryAndGroupMandatory(userDefinedFieldListValuesDTOs);
		logger.info("############ UserDefinedFields :: Processing DONE  ############");
	}

	/**
	 * Check if all fields are not mandatory and group mandatory.
	 *
	 * @author ManelLamloum
	 * @param definedFieldsDTOs the defined fields DTOs
	 */
	void checkIfAllFieldsNotMandatoryAndGroupMandatory(
			List<UserDefinedFieldsDTO> definedFieldsDTOs) {

		Map<String, List<UserDefinedFieldsDTO>> fieldGroupMap = definedFieldsDTOs.stream()
				.collect(Collectors.groupingBy(userGroupUdfParam -> userGroupUdfParam
						.getUserDefinedFieldGroupDTO().getCode()));
		UserDTO userDTO = CommonFunctions.getConnectedUser(logger);
		for (Map.Entry<String, List<UserDefinedFieldsDTO>> entry : fieldGroupMap.entrySet()) {
			Integer groupNotMondatory = 0;
			UserDefinedFieldGroupDTO definedFieldGroupDTO =
					entry.getValue().get(0).getUserDefinedFieldGroupDTO();
			// count the number of non mandatory fields in each group
			for (UserDefinedFieldsDTO udf : entry.getValue()) {
				if (udf.getMandatory().equals(Boolean.FALSE)) {
					groupNotMondatory++;
				}
			}
			// if all fields of udfGroup is not mandatory and udfGroup is mandatory then
			// set udfGroup to notMondatory
			if (groupNotMondatory == entry.getValue().size()
					&& definedFieldGroupDTO.getMondatory().equals(Boolean.TRUE)) {
				definedFieldGroupDTO.setMondatory(Boolean.FALSE);
				userDefinedFieldGroupService.save(definedFieldGroupDTO, userDTO);
			}
		}
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserDefinedFieldsService#findUDFFieldByListIds(java.util.List)
	 */
	@Override
	public List<UserDefinedFieldsDTO> findUDFFieldByListIds(
			List<UserDefinedFieldsDTO> userDefinedFieldsDTOs) {

		List<UserDefinedFieldsDTO> userDefinedFieldssDTOs = new ArrayList<>();
		// check if list udf field not empty
		if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldsDTOs)) {
			// init list ids
			ArrayList<Long> ids = new ArrayList<Long>();
			userDefinedFieldsDTOs.forEach(udfDTO -> ids.add(udfDTO.getIdUDFField()));
			List<UserDefinedFields> listUserDefinedFields =
					userDefinedFieldsRepository.findByIdUDFFieldInAndEnabled(ids, Boolean.TRUE);
			logger.info("{} : UserDefinedFields was founded", listUserDefinedFields.size());
			listUserDefinedFields = listUserDefinedFields.stream()
					.sorted(Comparator.comparingLong(
							udf -> udf.getOrdre() != null ? udf.getOrdre() : udf.getId()))
					.collect(Collectors.toList());
			// mapping data && loading list value if exist

			listUserDefinedFields.forEach(userDefinedFields -> {
				UserDefinedFieldsDTO dto =
						mapper.map(userDefinedFields, UserDefinedFieldsDTO.class);
				if (dto.getFieldType().equals(SettingUDFFieldsType.LIST.typeId())
						&& dto.getIdUDFListValue() != 0) {
					dto.getFieldListValuesDTOs().addAll(userDefinedFieldListValuesService.find(
							new UserDefinedFieldListValuesDTO(dto.getIdUDFListValue(), null)));
				}
				userDefinedFieldssDTOs.add(dto);
			});
		}
		return userDefinedFieldssDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserDefinedFieldsService#findUDFFieldByIds(java.util.List)
	 */
	@Override
	public List<UserDefinedFieldsDTO> findUDFFieldByIds(List<Long> ids) {

		Preconditions.checkNotNull(ids, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find UserDefinedField by IDs : {}", ids);
		// find udf fields by ids and enabled
		List<UserDefinedFields> userDefinedFields =
				userDefinedFieldsRepository.findByIdInAndEnabled(ids, Boolean.TRUE);
		userDefinedFields = userDefinedFields.stream()
				.sorted(Comparator.comparingLong(
						udf -> udf.getOrdre() != null ? udf.getOrdre() : udf.getId()))
				.collect(Collectors.toList());
		return userDefinedFields.stream()
				.map(userDefinedField -> mapper.map(userDefinedField, UserDefinedFieldsDTO.class))
				.collect(Collectors.toList());
	}

}
