/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import javax.persistence.EntityManager;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Service;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.support.TransactionTemplate;

import com.acm.ApplicationProperties;
import com.acm.client.TransversClient;
import com.acm.client.UserClient;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.UserDefinedFieldListValuesRepository;
import com.acm.repository.UserDefinedFieldsRepository;
import com.acm.service.UserDefinedFieldGroupService;
import com.acm.service.UserDefinedFieldListValuesService;
import com.acm.service.UserDefinedFieldsService;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.dtos.UserDefinedFieldListValuesDTO;
import com.acm.utils.enums.SettingUDFFieldsType;
import com.acm.utils.models.QUserDefinedFieldListValues;
import com.acm.utils.models.UserDefinedFieldListValues;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link UserDefinedFieldListValuesServiceImpl} Class Impl.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
@Service
public class UserDefinedFieldListValuesServiceImpl implements UserDefinedFieldListValuesService {

	/** The Constant logger. */
	private static final Logger logger =
			LoggerFactory.getLogger(UserDefinedFieldListValuesServiceImpl.class);

	/** The userDefinedFieldListValues repository. */
	@Autowired
	private UserDefinedFieldListValuesRepository userDefinedFieldListValuesRepository;

	/** The user defined field group service. */
	@Autowired
	private UserDefinedFieldGroupService userDefinedFieldGroupService;

	/** The user defined fields service. */
	@Autowired
	private UserDefinedFieldsService userDefinedFieldsService;

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

	/** The entity manager. */
	@Autowired
	private EntityManager entityManager;

	/** The transaction manager. */
	@Autowired
	private PlatformTransactionManager transactionManager;

	/** The root location. */
	private final Path rootLocation;

	/** The user defined fields repository. */
	@Autowired
	private UserDefinedFieldsRepository userDefinedFieldsRepository;

	/**
	 * Instantiates a new user defined field list values service impl.
	 *
	 * @param properties the properties
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	public UserDefinedFieldListValuesServiceImpl(ApplicationProperties properties)
			throws IOException {

		this.rootLocation = Paths.get(properties.getStorageLocation().getURL().getPath());
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserDefinedFieldListValuesService#find(java.lang.Long)
	 */
	@Override
	public UserDefinedFieldListValuesDTO find(Long id) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find UserDefinedFieldListValues by ID : {}", id);
		UserDefinedFieldListValues userDefinedFieldListValues =
				userDefinedFieldListValuesRepository.findById(id).orElse(null);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(userDefinedFieldListValues)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					UserDefinedFieldListValues.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty("exception.message.not.found")
							+ UserDefinedFieldListValues.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		return mapper.map(userDefinedFieldListValues, UserDefinedFieldListValuesDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserDefinedFieldListValuesService#find(com.acm.utils.dtos.
	 * UserDefinedFieldListValuesDTO)
	 */
	@Override
	public List<UserDefinedFieldListValuesDTO> find(
			UserDefinedFieldListValuesDTO userDefinedFieldListValuesDTO) {

		Preconditions.checkNotNull(userDefinedFieldListValuesDTO,
				CommonExceptionsMessage.EXCEPTIONS_ID_NULL);

		// init QUserDefinedFieldListValues
		QUserDefinedFieldListValues qUserDefinedFieldListValues =
				QUserDefinedFieldListValues.userDefinedFieldListValues;
		// init Predicate
		BooleanBuilder predicate =
				buildQuery(userDefinedFieldListValuesDTO, qUserDefinedFieldListValues);

		// check predicate if NULL
		if (predicate == null) {
			return new ArrayList<>();
		}
		Iterable<UserDefinedFieldListValues> iterable =
				userDefinedFieldListValuesRepository.findAll(predicate);
		List<UserDefinedFieldListValues> userDefinedFieldListValuess = new ArrayList<>();
		iterable.forEach(userDefinedFieldListValuess::add);
		logger.info("{} : UserDefinedFieldListValues was founded",
				userDefinedFieldListValuess.size());

		List<UserDefinedFieldListValuesDTO> userDefinedFieldListValuessDTOs = new ArrayList<>();
		userDefinedFieldListValuess
				.forEach(userDefinedFieldListValues -> userDefinedFieldListValuessDTOs.add(mapper
						.map(userDefinedFieldListValues, UserDefinedFieldListValuesDTO.class)));
		return userDefinedFieldListValuessDTOs;
	}

	/**
	 * Builds the query.
	 *
	 * @author HaythemBenizid
	 * @param userDefinedFieldListValuesDTO the user defined field list values DTO
	 * @param qUserDefinedFieldListValues the q user defined field list values
	 * @return the boolean builder
	 */
	private BooleanBuilder buildQuery(UserDefinedFieldListValuesDTO userDefinedFieldListValuesDTO,
			QUserDefinedFieldListValues qUserDefinedFieldListValues) {

		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// check if data not NULL
		if (ACMValidationUtils.isNullOrEmpty(userDefinedFieldListValuesDTO.getIdUDFListLink())
				&& ACMValidationUtils
						.isNullOrEmpty(userDefinedFieldListValuesDTO.getIdUDFListValue())
				&& ACMValidationUtils
						.isNullOrEmpty(userDefinedFieldListValuesDTO.getParentUDFListValue())
				&& ACMValidationUtils.isNullOrEmpty(userDefinedFieldListValuesDTO.getName())
				&& ACMValidationUtils.isNullOrEmpty(userDefinedFieldListValuesDTO.getId())
				&& ACMValidationUtils.isNullOrEmpty(userDefinedFieldListValuesDTO.getIdUDFList())) {
			return null;
		}

		// find only enabled data
		predicate.and(qUserDefinedFieldListValues.enabled.eq(Boolean.TRUE));

		// find by parent UDF List Value
		if (!ACMValidationUtils
				.isNullOrEmpty(userDefinedFieldListValuesDTO.getParentUDFListValue())) {
			predicate.and(qUserDefinedFieldListValues.parentUDFListValue
					.eq(userDefinedFieldListValuesDTO.getParentUDFListValue() != null
							? userDefinedFieldListValuesDTO.getParentUDFListValue()
							: 0));
		}

		// find by id
		if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldListValuesDTO.getId())) {
			predicate.and(qUserDefinedFieldListValues.id.eq(userDefinedFieldListValuesDTO.getId()));
		}

		// find by id UDF List
		if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldListValuesDTO.getIdUDFList())) {
			predicate.and(qUserDefinedFieldListValues.idUDFList
					.eq(userDefinedFieldListValuesDTO.getIdUDFList()));
		}

		// find by table Abacus Name
		if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldListValuesDTO.getTableAbacusName())) {
			predicate.and(qUserDefinedFieldListValues.tableAbacusName
					.eq(userDefinedFieldListValuesDTO.getTableAbacusName()));
		}

		// find by idUDFListLink
		if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldListValuesDTO.getIdUDFListLink())) {
			predicate.and(qUserDefinedFieldListValues.idUDFListLink
					.eq(userDefinedFieldListValuesDTO.getIdUDFListLink()));
		}

		// find by id UDListe Value
		if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldListValuesDTO.getIdUDFListValue())) {
			predicate.and(qUserDefinedFieldListValues.idUDFListValue
					.eq(userDefinedFieldListValuesDTO.getIdUDFListValue()));
		}
		// find by name
		if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldListValuesDTO.getName())) {
			predicate.and(
					qUserDefinedFieldListValues.name.eq(userDefinedFieldListValuesDTO.getName()));
		}
		return predicate;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserDefinedFieldListValuesService#save(com.acm.utils.dtos.
	 * UserDefinedFieldListValuesDTO)
	 */
	@Override
	public UserDefinedFieldListValuesDTO save(
			UserDefinedFieldListValuesDTO userDefinedFieldListValuesDTO, UserDTO userDTO) {

		Preconditions.checkNotNull(userDefinedFieldListValuesDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		UserDefinedFieldListValues userDefinedFieldListValues =
				mapper.map(userDefinedFieldListValuesDTO, UserDefinedFieldListValues.class);

		userDefinedFieldListValues.setInsertBy(userDTO.getFullName());
		userDefinedFieldListValues.setAcmVersion(0);
		userDefinedFieldListValues.setDateInsertion(new Date());
		userDefinedFieldListValues.setEnabled(userDefinedFieldListValuesDTO.getEnabled());

		UserDefinedFieldListValues newUserDefinedFieldListValues =
				userDefinedFieldListValuesRepository.save(userDefinedFieldListValues);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE,
				UserDefinedFieldListValues.class.getSimpleName());
		return mapper.map(newUserDefinedFieldListValues, UserDefinedFieldListValuesDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserDefinedFieldListValuesService#saveAll(java.util.List,
	 * com.acm.utils.dtos.UserDTO)
	 */
	@Override
	public List<UserDefinedFieldListValuesDTO> saveAll(
			List<UserDefinedFieldListValuesDTO> userDefinedFieldListValuesDTOs, UserDTO userDTO) {

		List<UserDefinedFieldListValues> UserDefinedFieldListValuesList =
				new ArrayList<UserDefinedFieldListValues>();
		List<UserDefinedFieldListValuesDTO> newUserDefinedFieldListValuesDTOs =
				new ArrayList<UserDefinedFieldListValuesDTO>();

		for (UserDefinedFieldListValuesDTO item : userDefinedFieldListValuesDTOs) {
			Preconditions.checkNotNull(item, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
			UserDefinedFieldListValues userDefinedFieldListValues =
					mapper.map(item, UserDefinedFieldListValues.class);

			if (ACMValidationUtils.isNullOrEmpty(userDTO.getNom())) {
				userDTO = CommonFunctions.getConnectedUser(logger);
			}

			userDefinedFieldListValues.setInsertBy(userDTO.getFullName());
			userDefinedFieldListValues.setAcmVersion(0);
			userDefinedFieldListValues.setDateInsertion(new Date());
			userDefinedFieldListValues.setEnabled(item.getEnabled());
			UserDefinedFieldListValuesList.add(userDefinedFieldListValues);

			newUserDefinedFieldListValuesDTOs.add(
					mapper.map(userDefinedFieldListValues, UserDefinedFieldListValuesDTO.class));
		}

		userDefinedFieldListValuesRepository.saveAll(UserDefinedFieldListValuesList);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE,
				UserDefinedFieldListValues.class.getSimpleName());

		return userDefinedFieldListValuesDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserDefinedFieldListValuesService#save(java.lang.Long,
	 * com.acm.utils.dtos.UserDefinedFieldListValuesDTO)
	 */
	@Override
	public UserDefinedFieldListValuesDTO save(Long id,
			UserDefinedFieldListValuesDTO userDefinedFieldListValuesDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(userDefinedFieldListValuesDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.info("Update UserDefinedFieldListValues with ID = {}", id);
		UserDefinedFieldListValues oldUserDefinedFieldListValues =
				userDefinedFieldListValuesRepository.findById(id).orElse(null);

		// check if object is null
		if (oldUserDefinedFieldListValues == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					UserDefinedFieldListValues.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND
							+ UserDefinedFieldListValues.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		// mapping new data with existing data (oldUserDefinedFieldListValues)
		mapper.map(userDefinedFieldListValuesDTO, oldUserDefinedFieldListValues);
		CommonFunctions.mapperToUpdate(oldUserDefinedFieldListValues, userClient, logger);

		// update & persist data in DB
		UserDefinedFieldListValues newUserDefinedFieldListValues =
				userDefinedFieldListValuesRepository.save(oldUserDefinedFieldListValues);

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE,
				UserDefinedFieldListValues.class.getSimpleName());
		return mapper.map(newUserDefinedFieldListValues, UserDefinedFieldListValuesDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserDefinedFieldListValuesService#loadSettingFromAbacus()
	 */
	@Override
	public void loadSettingFromAbacus(UserDTO userDTO) {

		List<UserDefinedFieldListValuesDTO> userDefinedFieldListValuesDTOs =
				settingTransversClient.findUserDefinedFieldListValues();
		saveAll(userDefinedFieldListValuesDTOs, userDTO);
		logger.info("############ UserDefinedFieldListValues :: INSERT DONE ############");
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserDefinedFieldListValuesService#resetSettingFromAbacus()
	 */
	@Override
	public void resetSettingFromAbacus() throws ResourcesNotFoundException {

		logger.info("Method : UDF resetSettingFromAbacus() :: Start");

		// Delete User Defined Field List Values of abacus udf fields
		deleteUdfListValuesOfAbacusUdfFields();
		// Reset Primary Key Value to "1" => Table ACM_UDF_LIST_VALUES
		executeQuery("DBCC CHECKIDENT (ACM_UDF_LIST_VALUES, RESEED, 0)");
		// connected user
		UserDTO userDTO = CommonFunctions.getConnectedUser(logger);
		// Load Setting UDF group
		userDefinedFieldGroupService.loadSettingFromAbacus(userDTO);

		// Load setting UDF fields
		userDefinedFieldsService.loadSettingFromAbacus(userDTO);

		// Load UDF List Values
		loadSettingFromAbacus(userDTO);

		/*
		 * Execute update query on Table : ACM_UDF_LIST_VALUES && ACM_UDF_FIELD
		 */
		// Load list update query from external file : setting_update_data.sql
		List<String> lines = Collections.emptyList();
		try {
			lines = Files.readAllLines(rootLocation.resolve("setting_update_data.sql"),
					StandardCharsets.UTF_8);
		}
		catch (IOException e) {
			logger.error(e.getMessage());
			e.printStackTrace();
		}
		// Execute UPDATE query
		for (String query : lines) {
			if (!ACMValidationUtils.isNullOrEmpty(query)) {
				executeQuery(query);
				logger.debug("{} :: DONE", query);
			}
		}
		logger.info("Method : UDF resetSettingFromAbacus() :: DONE");
	}

	/**
	 * Delete udf list values of abacus udf fields.
	 */
	private void deleteUdfListValuesOfAbacusUdfFields() {

		// find IdUDFListValues of abacus udf fields
		List<Long> idUDFListValues =
				userDefinedFieldsRepository.findIdUDFListValueByIdUDFFieldIsNotAndFieldType(0L,
						SettingUDFFieldsType.LIST.typeId());

		userDefinedFieldListValuesRepository.deleteByIdUDFListInOrIdUDFListLinkIn(idUDFListValues,
				idUDFListValues);
	}

	/**
	 * Execute given update query on TABLE.
	 *
	 * @author HaythemBenizid
	 * @param query the query
	 * @return the integer
	 */
	public Integer executeQuery(String query) {

		// executing given query
		TransactionTemplate transactionTemplate = new TransactionTemplate(transactionManager);
		return transactionTemplate.execute(transactionStatus -> {
			int status = entityManager.createNativeQuery(query).executeUpdate();
			transactionStatus.flush();
			return status;
		});
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserDefinedFieldListValuesService#findByIdUDFListValue(java.lang.Long)
	 */
	@Override
	public List<UserDefinedFieldListValuesDTO> findByIdUDFListValue(Long idUDFListValue) {

		Preconditions.checkNotNull(idUDFListValue, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("find By IdUDFListValue And Enabled : {}", idUDFListValue);
		List<UserDefinedFieldListValues> userDefinedFieldListValues =
				userDefinedFieldListValuesRepository.findByIdUDFListValueAndEnabled(idUDFListValue,
						Boolean.TRUE);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(userDefinedFieldListValues)) {
			return new ArrayList<>();
		}
		// mapping && returning data
		List<UserDefinedFieldListValuesDTO> userDefinedFieldListValuesDTOs = new ArrayList<>();
		userDefinedFieldListValues.forEach(udf -> userDefinedFieldListValuesDTOs
				.add(mapper.map(udf, UserDefinedFieldListValuesDTO.class)));
		logger.info("METHOD : findByIdUDFListValueAndEnabled : {} : UDFListValues was founded",
				userDefinedFieldListValuesDTOs.size());
		return userDefinedFieldListValuesDTOs;
	}

}
