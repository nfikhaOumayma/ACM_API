/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.util.ArrayList;
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

import com.acm.client.TransversClient;
import com.acm.client.UserClient;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.AddressSettingRepository;
import com.acm.service.AddressSettingService;
import com.acm.utils.dtos.AddressListDTO;
import com.acm.utils.dtos.AddressListValueDTO;
import com.acm.utils.dtos.AddressSettingAbacusDTO;
import com.acm.utils.dtos.AddressSettingDTO;
import com.acm.utils.dtos.AddressTypeDTO;
import com.acm.utils.enums.SettingAddressTable;
import com.acm.utils.models.AddressSetting;
import com.acm.utils.models.QAddressSetting;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link AddressSettingServiceImpl} Class Impl.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
@Service
public class AddressSettingServiceImpl implements AddressSettingService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(AddressSettingServiceImpl.class);

	/** The addressSetting repository. */
	@Autowired
	private AddressSettingRepository addressSettingRepository;

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

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AddressSettingService#find(java.lang.Long)
	 */
	@Override
	public AddressSettingDTO find(Long id) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find AddressSetting by ID : {}", id);
		AddressSetting addressSetting = addressSettingRepository.findById(id).orElse(null);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(addressSetting)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					AddressSetting.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty("exception.message.not.found")
							+ AddressSetting.class.getSimpleName() + CommonExceptionsMessage.WITH_ID
							+ id);
		}
		return mapper.map(addressSetting, AddressSettingDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AddressSettingService#find(com.acm.utils.dtos.AddressSettingDTO)
	 */
	@Override
	public List<AddressSettingDTO> find(AddressSettingDTO addressSettingDTO) {

		Preconditions.checkNotNull(addressSettingDTO, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);

		// init QAddressSetting
		QAddressSetting qAddressSetting = QAddressSetting.addressSetting;
		// init Predicate
		BooleanBuilder predicate = buildQuery(addressSettingDTO, qAddressSetting);

		Iterable<AddressSetting> iterable = addressSettingRepository.findAll(predicate);
		List<AddressSetting> addressSettings = new ArrayList<>();
		iterable.forEach(addressSettings::add);
		logger.info("{} : AddressSetting was founded", addressSettings.size());

		List<AddressSettingDTO> addressSettingsDTOs = new ArrayList<>();
		addressSettings.forEach(addressSetting -> addressSettingsDTOs
				.add(mapper.map(addressSetting, AddressSettingDTO.class)));
		return addressSettingsDTOs;
	}

	/**
	 * Builds the query.
	 *
	 * @author HaythemBenizid
	 * @param addressSettingDTO the address setting DTO
	 * @param qAddressSetting the q address setting
	 * @return the boolean builder
	 */
	private BooleanBuilder buildQuery(AddressSettingDTO addressSettingDTO,
			QAddressSetting qAddressSetting) {

		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find only enabled data
		predicate.and(qAddressSetting.enabled.eq(Boolean.TRUE));

		// find by table Abacus Name
		if (!ACMValidationUtils.isNullOrEmpty(addressSettingDTO.getTableAbacusName())) {
			predicate.and(
					qAddressSetting.tableAbacusName.eq(addressSettingDTO.getTableAbacusName()));
		}

		// find by address List Id
		if (!ACMValidationUtils.isNullOrEmpty(addressSettingDTO.getAddressListId())) {
			predicate.and(qAddressSetting.addressListId.eq(addressSettingDTO.getAddressListId()));
		}

		// find by Ids address List
		if (!ACMValidationUtils.isNullOrEmpty(addressSettingDTO.getIdAddressList())) {
			predicate.and(qAddressSetting.idExtern.in(addressSettingDTO.getIdAddressList()));
		}

		// find by ID extern
		if (!ACMValidationUtils.isNullOrEmpty(addressSettingDTO.getIdExtern())) {
			predicate.and(qAddressSetting.idExtern.eq(addressSettingDTO.getIdExtern()));
		}

		// find by parent ID
		if (!ACMValidationUtils.isNullOrEmpty(addressSettingDTO.getParentId())) {
			predicate.and(qAddressSetting.parentId.eq(addressSettingDTO.getParentId()));
		}
		return predicate;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AddressSettingService#save(com.acm.utils.dtos.AddressSettingDTO)
	 */
	@Override
	public AddressSettingDTO save(AddressSettingDTO addressSettingDTO) {

		Preconditions.checkNotNull(addressSettingDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		AddressSetting addressSetting = mapper.map(addressSettingDTO, AddressSetting.class);

		CommonFunctions.mapperToSave(addressSetting, userClient, logger);
		AddressSetting newAddressSetting = addressSettingRepository.save(addressSetting);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, AddressSetting.class.getSimpleName());
		return mapper.map(newAddressSetting, AddressSettingDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AddressSettingService#save(java.lang.Long,
	 * com.acm.utils.dtos.AddressSettingDTO)
	 */
	@Override
	public AddressSettingDTO save(Long id, AddressSettingDTO addressSettingDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(addressSettingDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.info("Update AddressSetting with ID = {}", id);
		AddressSetting oldAddressSetting = addressSettingRepository.findById(id).orElse(null);

		// check if object is null
		if (oldAddressSetting == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					AddressSetting.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + AddressSetting.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		// mapping new data with existing data (oldAddressSetting)
		mapper.map(addressSettingDTO, oldAddressSetting);
		CommonFunctions.mapperToUpdate(oldAddressSetting, userClient, logger);

		// update & persist data in DB
		AddressSetting newAddressSetting = addressSettingRepository.save(oldAddressSetting);

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, AddressSetting.class.getSimpleName());
		return mapper.map(newAddressSetting, AddressSettingDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AddressSettingService#loadSettingFromAbacus()
	 */
	@Override
	public void loadSettingFromAbacus() {

		List<AddressTypeDTO> addressTypeDTOs = settingTransversClient.findAddressType();
		List<AddressSettingDTO> AddressSettingDTOLst = new ArrayList<AddressSettingDTO>();
		for (AddressTypeDTO addressTypeDTO : addressTypeDTOs) {
			logger.debug("{}", addressTypeDTO);
			AddressSettingDTO addressSettingDTO =
					new AddressSettingDTO(null, SettingAddressTable.ADDRESS_TYPE.tableName(),
							String.valueOf(addressTypeDTO.getAddressTypeID()),
							CommonFunctions.convertObjectToJSONString(addressTypeDTO), 0L);
			logger.debug("{}", addressSettingDTO);
			AddressSettingDTOLst.add(addressSettingDTO);
			// save(addressSettingDTO);
		}

		logger.info("############AddressTypeDTO  :: INSERT DONE");

		List<AddressListDTO> addressListDTOs = settingTransversClient.findAddressList();
		for (AddressListDTO addressListDTO : addressListDTOs) {
			logger.debug("{}", addressListDTO);

			AddressSettingDTO addressSettingDTO =
					new AddressSettingDTO(addressListDTO.getParentAddressListID().longValue(),
							SettingAddressTable.ADDRESS_LIST.tableName(),
							String.valueOf(addressListDTO.getAddressListID()),
							CommonFunctions.convertObjectToJSONString(addressListDTO), 0L);
			logger.debug("{}", addressSettingDTO);
			AddressSettingDTOLst.add(addressSettingDTO);
			// save(addressSettingDTO);
		}
		logger.info("############AddressListDTO  :: INSERT DONE");

		List<AddressListValueDTO> addressListValueDTOs =
				settingTransversClient.findAddressListValue();
		for (AddressListValueDTO addressListValueDTO : addressListValueDTOs) {
			logger.debug("{}", addressListValueDTO);

			AddressSettingDTO addressSettingDTO = new AddressSettingDTO(
					addressListValueDTO.getParentAddressListValueID() != null
							? addressListValueDTO.getParentAddressListValueID().longValue()
							: null,
					SettingAddressTable.ADDRESS_LIST_VALUE.tableName(),
					String.valueOf(addressListValueDTO.getAddressListValueID()),
					CommonFunctions.convertObjectToJSONString(addressListValueDTO),
					addressListValueDTO.getAddressListID().longValue());
			logger.debug("{}", addressSettingDTO);
			AddressSettingDTOLst.add(addressSettingDTO);

			// save(addressSettingDTO);
		}
		logger.info("############AddressListValueDTO  :: INSERT DONE");

		List<AddressSettingAbacusDTO> addressSettingsDTOs =
				settingTransversClient.findSettingsAddress();
		for (AddressSettingAbacusDTO addressSettingsDTO : addressSettingsDTOs) {
			logger.debug("{}", addressSettingsDTO);

			AddressSettingDTO addressSettingDTO =
					new AddressSettingDTO(null, SettingAddressTable.SETTINGS_ADDRESS.tableName(),
							addressSettingsDTO.getAddressField(),
							CommonFunctions.convertObjectToJSONString(addressSettingsDTO), 0L);
			logger.debug("{}", addressSettingDTO);
			AddressSettingDTOLst.add(addressSettingDTO);

		}
		logger.info("############AddressSettingsDTO  :: INSERT DONE");
		saveAll(AddressSettingDTOLst);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AddressSettingService#resetSettingFromAbacus()
	 */
	@Override
	public void resetSettingFromAbacus() {

		logger.info("Method address_Setting : resetSettingFromAbacus() :: Start");
		// remove All data from DB
		addressSettingRepository.deleteAll();

		// Reset Primary Key Value to "1"
		int status = resetPrimaryKey();
		logger.info("Reset Primary Key Value :: DONE with status = {} ", status);

		// reload ADDRESS setting
		loadSettingFromAbacus();

		logger.info("Method address_Setting : resetSettingFromAbacus() :: DONE");
	}

	/**
	 * Reset primary key to start from : "1".
	 * 
	 * @author HaythemBenizid
	 * @return the integer
	 */
	public Integer resetPrimaryKey() {

		TransactionTemplate transactionTemplate = new TransactionTemplate(transactionManager);
		return transactionTemplate.execute(transactionStatus -> {
			int status = entityManager
					.createNativeQuery("DBCC CHECKIDENT (ACM_ADDRESS_SETTING, RESEED, 0)")
					.executeUpdate();
			transactionStatus.flush();
			return status;
		});
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AddressSettingService#saveAll(java.util.List)
	 */
	@Override
	public AddressSettingDTO saveAll(List<AddressSettingDTO> addressSettingDTOs) {

		List<AddressSetting> listAddressSetting = new ArrayList<AddressSetting>();
		addressSettingDTOs.forEach((item) -> {
			Preconditions.checkNotNull(item, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
			AddressSetting addressSetting = mapper.map(item, AddressSetting.class);

			CommonFunctions.mapperToSave(addressSetting, userClient, logger);
			listAddressSetting.add(addressSetting);

		});
		List<AddressSetting> newAddressSetting =
				addressSettingRepository.saveAll(listAddressSetting);
		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, AddressSetting.class.getSimpleName());
		return mapper.map(newAddressSetting, AddressSettingDTO.class);
	}
}
