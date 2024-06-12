/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Service;

import com.acm.aop.history.ProcessHistorySetting;
import com.acm.client.UserClient;
import com.acm.constants.common.CommonAOPConstants;
import com.acm.constants.common.CommonConstants;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.AcmEnvironnementService;
import com.acm.service.SettingHistoriqueService;
import com.acm.utils.dtos.AcmEnvironnementDTO;
import com.acm.utils.dtos.GroupeDTO;
import com.acm.utils.dtos.SettingHistoriqueDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.enums.SettingCategory;
import com.acm.utils.models.AcmEnvironnement;
import com.acm.utils.models.QAcmEnvironnement;
import com.acm.utils.repository.AcmEnvironnementRepository;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link AcmEnvironnementServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
@Service
public class AcmEnvironnementServiceImpl implements AcmEnvironnementService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(AcmEnvironnementServiceImpl.class);

	/** The acmEnvironnement repository. */
	@Autowired
	private AcmEnvironnementRepository acmEnvironnementRepository;

	/** The setting historique service. */
	@Autowired
	private SettingHistoriqueService settingHistoriqueService;

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
	 * @see com.acm.service.AcmEnvironnementService#find(java.lang.String)
	 */
	@Override
	public AcmEnvironnementDTO find(String key) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(key, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.debug("Find AcmEnvironnement by Key : {}", key);
		List<AcmEnvironnement> acmEnvironnements = acmEnvironnementRepository.findByKey(key);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(acmEnvironnements)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					AcmEnvironnement.class.getSimpleName());
			logger.error("{} {} {} {}", environment.getProperty("exception.message.not.found"),
					AcmEnvironnement.class.getSimpleName(), CommonExceptionsMessage.WITH_ID, key);
			return null;
		}
		logger.debug("AcmEnvironnement value : {}", acmEnvironnements.get(0));
		return mapper.map(acmEnvironnements.get(0), AcmEnvironnementDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmEnvironnementService#save(com.acm.utils.dtos.AcmEnvironnementDTO)
	 */
	@Override
	@ProcessHistorySetting(action = CommonAOPConstants.NEW)
	public AcmEnvironnementDTO save(AcmEnvironnementDTO acmEnvironnementDTO) {

		Preconditions.checkNotNull(acmEnvironnementDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		AcmEnvironnement acmEnvironnement = mapper.map(acmEnvironnementDTO, AcmEnvironnement.class);
		CommonFunctions.mapperToSave(acmEnvironnement, userClient, logger);
		AcmEnvironnement newAcmEnvironnement = acmEnvironnementRepository.save(acmEnvironnement);

		logger.debug(CommonLoggerMessage.SUCCESFULL_CREATE, AcmEnvironnement.class.getSimpleName());
		return mapper.map(newAcmEnvironnement, AcmEnvironnementDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmEnvironnementService#save(java.lang.Long,
	 * com.acm.utils.dtos.AcmEnvironnementDTO)
	 */
	@Override
	public AcmEnvironnementDTO save(Long id, AcmEnvironnementDTO acmEnvironnementDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(acmEnvironnementDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.debug("Update acmEnvironnement  with ID = {}", id);
		AcmEnvironnement oldAcmEnvironnement = acmEnvironnementRepository.findById(id).orElse(null);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(oldAcmEnvironnement)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					AcmEnvironnement.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + AcmEnvironnement.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}

		// init settingHistory object
		SettingHistoriqueDTO settingHistoriqueDTO = new SettingHistoriqueDTO(
				CommonFunctions.getTableNameFromClass(AcmEnvironnement.class),
				CommonAOPConstants.UPDATE, id,
				CommonFunctions.convertObjectToJSONString(oldAcmEnvironnement));

		// mapping new data with existing data (oldAcmEnvironnement)
		mapper.map(acmEnvironnementDTO, oldAcmEnvironnement);
		CommonFunctions.mapperToUpdate(oldAcmEnvironnement, userClient, logger);
		AcmEnvironnement newAcmEnvironnement = acmEnvironnementRepository.save(oldAcmEnvironnement);

		// saving history setting
		settingHistoriqueDTO.setUpdatedBy(newAcmEnvironnement.getUpdatedBy());
		settingHistoriqueDTO
				.setNewData(CommonFunctions.convertObjectToJSONString(newAcmEnvironnement));
		settingHistoriqueService.save(settingHistoriqueDTO);

		logger.debug(CommonLoggerMessage.SUCCESFULL_UPDATE, AcmEnvironnement.class.getSimpleName());
		return mapper.map(newAcmEnvironnement, AcmEnvironnementDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmEnvironnementService#find(com.acm.utils.dtos.AcmEnvironnementDTO)
	 */
	@Override
	public List<AcmEnvironnementDTO> find(AcmEnvironnementDTO acmEnvironnementDTO) {

		// init QAcmEnvironnement
		QAcmEnvironnement qAcmEnvironnement = QAcmEnvironnement.acmEnvironnement;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find only enabled data
		predicate.and(qAcmEnvironnement.enabled.eq(Boolean.TRUE));

		// find by key starting with given params
		if (!ACMValidationUtils.isNullOrEmpty(acmEnvironnementDTO.getKey())
				&& Boolean.FALSE.equals(acmEnvironnementDTO.getSearchSettingAML())) {
			predicate.and(qAcmEnvironnement.key.eq(acmEnvironnementDTO.getKey()));
		}
		// find by key starting with given params (used to search AML setting)
		if (!ACMValidationUtils.isNullOrEmpty(acmEnvironnementDTO.getKey())
				&& Boolean.TRUE.equals(acmEnvironnementDTO.getSearchSettingAML())) {
			predicate.and(qAcmEnvironnement.key.like(acmEnvironnementDTO.getKey() + "%"));
		}
		// QueryDSL using springDATA
		Iterable<AcmEnvironnement> iterable = acmEnvironnementRepository.findAll(predicate);
		List<AcmEnvironnement> acmEnvironnements = new ArrayList<>();
		iterable.forEach(acmEnvironnements::add);
		logger.debug("{} : params was founded", acmEnvironnements.size());

		// mapping returned list
		List<AcmEnvironnementDTO> acmEnvironnementDTOs = new ArrayList<>();
		acmEnvironnements.forEach(acmEnvironnement -> acmEnvironnementDTOs
				.add(mapper.map(acmEnvironnement, AcmEnvironnementDTO.class)));

		logger.debug("Returning founded data ...");
		return acmEnvironnementDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmEnvironnementService#find()
	 */
	@Override
	public List<AcmEnvironnementDTO> find() {

		// find connected user
		UserDTO userDTO = CommonFunctions.getConnectedUser(logger);
		List<AcmEnvironnementDTO> acmEnvironnementsDTOs = new ArrayList<>();
		List<AcmEnvironnement> acmEnvironnements = new ArrayList<>();
		// if Admin find all
		if (CommonConstants.DEFAULT_USER.equals(userDTO.getLogin())) {
			acmEnvironnements = acmEnvironnementRepository.findAll();
		}
		// find by functional category
		else {
			acmEnvironnements =
					acmEnvironnementRepository.findByCategory(SettingCategory.FUNCTIONAL.name());

		}
		acmEnvironnements.forEach(acmEnvironnement -> acmEnvironnementsDTOs
				.add(mapper.map(acmEnvironnement, AcmEnvironnementDTO.class)));
		logger.debug("Returning ALL founded data ...");
		return acmEnvironnementsDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmEnvironnementService#updateLimite(java.lang.String, java.lang.String)
	 */
	@Override
	public AcmEnvironnementDTO updateLimite(String key, String limite) {

		try {
			// find environnement by key
			AcmEnvironnementDTO environnementDTO = find(key);
			environnementDTO.setValue(limite);
			// updating data
			AcmEnvironnementDTO updatedEnvironnementDTO =
					save(environnementDTO.getId(), environnementDTO);
			logger.debug("Last {} add from ABACUS-DB is : [{}]", key,
					updatedEnvironnementDTO.getValue());
			return updatedEnvironnementDTO;
		}
		catch (ResourcesNotFoundException e) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					AcmEnvironnement.class.getSimpleName());
			logger.error(e.getMessage());
		}
		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.AcmEnvironnementService#findLikeKey(com.acm.utils.dtos.AcmEnvironnementDTO)
	 */
	@Override
	public List<AcmEnvironnementDTO> findLikeKey(AcmEnvironnementDTO acmEnvironnementDTO) {

		// init list
		List<AcmEnvironnementDTO> acmEnvironnementDTOs = new ArrayList<>();
		if (!ACMValidationUtils.isNullOrEmpty(acmEnvironnementDTO)
				&& !ACMValidationUtils.isNullOrEmpty(acmEnvironnementDTO.getKey())) {
			// init QAcmEnvironnement
			QAcmEnvironnement qAcmEnvironnement = QAcmEnvironnement.acmEnvironnement;
			// init Predicate
			BooleanBuilder predicate = new BooleanBuilder();

			// find only enabled data
			predicate.and(qAcmEnvironnement.enabled.eq(Boolean.TRUE));

			// find by key starting with given params
			predicate.and(qAcmEnvironnement.key.like(acmEnvironnementDTO.getKey() + "%"));

			// QueryDSL using springDATA
			Iterable<AcmEnvironnement> iterable = acmEnvironnementRepository.findAll(predicate);
			List<AcmEnvironnement> acmEnvironnements = new ArrayList<>();
			iterable.forEach(acmEnvironnements::add);
			logger.debug("{} : params was founded", acmEnvironnements.size());

			// mapping returned list
			acmEnvironnements.forEach(acmEnvironnement -> acmEnvironnementDTOs
					.add(mapper.map(acmEnvironnement, AcmEnvironnementDTO.class)));
		}
		logger.debug("Returning founded data NB: {} ...", acmEnvironnementDTOs.size());
		return acmEnvironnementDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmEnvironnementService#findByKeys(java.util.List)
	 */
	@Override
	public List<AcmEnvironnementDTO> findByKeys(List<String> keys) {

		// init list
		List<AcmEnvironnementDTO> acmEnvironnementDTOs = new ArrayList<>();
		// find acmEnvironnements by list of Keys
		List<AcmEnvironnement> acmEnvironnements = acmEnvironnementRepository.findByKeyIn(keys);
		// mapping returned list
		acmEnvironnements.forEach(acmEnvironnement -> acmEnvironnementDTOs
				.add(mapper.map(acmEnvironnement, AcmEnvironnementDTO.class)));
		AcmEnvironnementDTO acmEnvironnementDTO = acmEnvironnementDTOs.stream()
				.filter(p -> p.getKey().equals(CommonConstants.ACM_KEY)).findFirst().orElse(null);
		if (acmEnvironnementDTO != null) {

			try {
				acmEnvironnementDTOs.get(acmEnvironnementDTOs.indexOf(acmEnvironnementDTO))
						.setCreptedKey(acmEnvironnementDTOs
								.get(acmEnvironnementDTOs.indexOf(acmEnvironnementDTO)).getValue());
				acmEnvironnementDTOs.get(acmEnvironnementDTOs.indexOf(acmEnvironnementDTO))
						.setValue(CommonFunctions.decryptWithPrivateKey(acmEnvironnementDTOs
								.get(acmEnvironnementDTOs.indexOf(acmEnvironnementDTO))
								.getValue()));
			}
			catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}

		logger.info("Returning founded data NB: {} ...", acmEnvironnementDTOs.size());
		return acmEnvironnementDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmEnvironnementService#findByCategory(java.lang.String)
	 */
	@Override
	public List<AcmEnvironnementDTO> findByCategory(String category) {

		// check if category is null
		Preconditions.checkNotNull(category, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find AcmEnvironnement by Category : {}", category);
		// init list
		List<AcmEnvironnementDTO> acmEnvironnementsDTOs = new ArrayList<>();
		// find acm Environnement By category
		List<AcmEnvironnement> acmEnvironnements =
				acmEnvironnementRepository.findByCategory(category);
		// mapping returned list
		acmEnvironnements.forEach(acmEnvironnement -> acmEnvironnementsDTOs
				.add(mapper.map(acmEnvironnement, AcmEnvironnementDTO.class)));

		return acmEnvironnementsDTOs;

	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmEnvironnementService#checkIfConnectedUserGroupIsAuthorized()
	 */
	@Override
	public Boolean checkAthorisationConnectedUser(String acmEnvironnementKey)
			throws ResourcesNotFoundException {

		// find connected user
		UserDTO userDTO = CommonFunctions.getConnectedUser(logger);
		// get code group Of connected user
		GroupeDTO groupConnectedUser = userDTO.getGroupes().stream().findFirst().orElse(null);
		String groupeCode = groupConnectedUser.getCode();
		// get AcmEnvironnements of Authorized groups to acces all buttons in ACM by Key
		AcmEnvironnementDTO acmEnvironnementDTO = find(acmEnvironnementKey);
		// split a comma separated String into a list of String
		List<String> authorizedGroups = Stream.of(acmEnvironnementDTO.getValue().split(","))
				.map(elem -> new String(elem)).collect(Collectors.toList());
		// check if the group of connected user exist in the Authorized groups
		return authorizedGroups.contains(groupeCode);
	}

}
