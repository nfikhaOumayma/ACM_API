/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.acm.client.UserClient;
import com.acm.constants.common.CommonConstants;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLicenceVariable;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.HabilitationRepository;
import com.acm.service.AcmEnvironnementService;
import com.acm.service.GroupeService;
import com.acm.service.HabilitationService;
import com.acm.utils.dtos.AcmEnvironnementDTO;
import com.acm.utils.dtos.GroupeDTO;
import com.acm.utils.dtos.HabilitationDTO;
import com.acm.utils.dtos.HabilitationIHMRouteDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.models.Groupe;
import com.acm.utils.models.Habilitation;
import com.acm.utils.models.HabilitationIHMRoute;
import com.acm.utils.models.QHabilitation;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * The {@link HabilitationServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 0.3.0
 */
@Service
public class HabilitationServiceImpl implements HabilitationService {

	/** Default Mode is INFO. */
	private static final Logger logger = LoggerFactory.getLogger(HabilitationServiceImpl.class);

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The habilitation repository. */
	@Autowired
	private HabilitationRepository habilitationRepository;

	/** The user client. */
	@Autowired
	private UserClient userClient;

	/** The groupe service. */
	@Autowired
	private GroupeService groupeService;

	/** The acm environnement service. */
	@Autowired
	private AcmEnvironnementService acmEnvironnementService;

	/** The habilitation DT os. */
	List<HabilitationDTO> habilitationDTOs = new ArrayList<>();

	/** The habilitations. */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.HabilitationService#find()
	 */
	List<Habilitation> habilitations = new ArrayList<>();

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.HabilitationService#find()
	 */
	@Override
	public List<HabilitationDTO> find() throws Exception {

		// find connected user details
		List<String> keyList = new ArrayList<String>();
		List<AcmEnvironnementDTO> environnementList = new ArrayList<AcmEnvironnementDTO>();
		AcmEnvironnementDTO acmEnvironnementDTO = null;
		habilitations = new ArrayList<>();

		keyList.add(CommonConstants.ACM_KEY);
		UserDTO connectedUser = CommonFunctions.getConnectedUser(logger);
		environnementList = acmEnvironnementService.findByKeys(keyList);
		acmEnvironnementDTO = environnementList.get(0);
		// get groupe of user
		String[] splitActivationKeyList = acmEnvironnementDTO.getValue().split(",");

		Date expiryDate = searchExpiryDate(splitActivationKeyList);

		logger.debug("{} : expiryDate ", expiryDate);

		Integer maxActiveUser = searchMaxActiveUser(splitActivationKeyList);
		logger.debug("{} :maxActiveUser", maxActiveUser);

		Integer simultaniousUser = searchSimultaniousUser(splitActivationKeyList);
		logger.debug("{} : simultaniousUser", simultaniousUser);
		logger.debug("{} : current simultanious User", userClient.getSimultaniousUser());

		String macAdressLicence = getMacAdressFromLicence(splitActivationKeyList);
		logger.debug("{} :macAdressLicence", macAdressLicence);

		logger.debug("{} :macAdressServer", CommonLicenceVariable.macAdressServer);

		habilitationDTOs = new ArrayList<>();
		GroupeDTO groupeDTO = connectedUser.getGroupes().iterator().next();
		habilitations
				.addAll(habilitationRepository.findByIdGroupeAndClientAndEnabledAndAcmHabilitation(
						groupeDTO.getId(), CommonConstants.APP_NAME, Boolean.TRUE,
						CommonConstants.CODE_HABILITATION_SETTING));

		if (!habilitations.isEmpty()) {
			habilitations.forEach(habilitation -> habilitationDTOs
					.add(mapper.map(habilitation, HabilitationDTO.class)));
		}


			if (ACMValidationUtils.isNullOrEmpty(groupeDTO)) {
				logger.warn(CommonLoggerMessage.NOT_FOUND_OBJECT, Groupe.class.getSimpleName());
				throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_ID_NULL,
						CommonExceptionsMessage.NOT_FOUND + Groupe.class.getSimpleName());
			}

			habilitations = habilitationRepository.findByGroupIdWithHabilitation(groupeDTO.getId(),
					CommonConstants.APP_NAME, Boolean.TRUE,
					getListOfModuleFromKey(splitActivationKeyList));
			logger.debug("{} : params was founded", habilitations.size());

			if (!habilitations.isEmpty()) {
				habilitations.forEach(habilitation -> habilitationDTOs
						.add(mapper.map(habilitation, HabilitationDTO.class)));
			}

		

		return habilitationDTOs;
	}

	/**
	 * Gets the list of module from key.
	 *
	 * @author kouali
	 * @param splitActivationKeyList the split activation key list
	 * @return the list of module from key
	 */
	private List<String> getListOfModuleFromKey(String[] splitActivationKeyList) {

		List<String> listModule = new ArrayList<String>();
		for (int i = 0; i < splitActivationKeyList.length; i++) {
			if (!splitActivationKeyList[i].contains(":")) {
				listModule.add(splitActivationKeyList[i].trim());
			}

		}
		return listModule;
	}

	/**
	 * Search expiry date.
	 *
	 * @author kouali
	 * @param splitActivationKeyList the split activation key list
	 * @return the date
	 * @throws ParseException the parse exception
	 */
	private Date searchExpiryDate(String[] splitActivationKeyList) throws ParseException {

		int index = 0;
		for (String s : splitActivationKeyList) {
			index++;
			if (s.contains(CommonConstants.EXPIRYDATE)) {
				break;
			}
		}
		SimpleDateFormat dateFormat = new SimpleDateFormat("ddMMyyyy");
		Date expiryDate = dateFormat.parse(splitActivationKeyList[index - 1].split(":")[1].trim());
		return expiryDate;
	}

	/**
	 * Search max active user.
	 *
	 * @param splitActivationKeyList the split activation key list
	 * @return the integer
	 * @throws ParseException the parse exception
	 */
	private Integer searchMaxActiveUser(String[] splitActivationKeyList) throws ParseException {

		int index = 0;
		for (String s : splitActivationKeyList) {
			index++;
			if (s.contains(CommonConstants.MAX_ACTIVE_USER)) {
				break;
			}
		}
		Integer maxActiveUser =
				Integer.parseInt(splitActivationKeyList[index - 1].split(":")[1].trim());
		return maxActiveUser;
	}

	/**
	 * Search simultanious user.
	 *
	 * @param splitActivationKeyList the split activation key list
	 * @return the integer
	 * @throws ParseException the parse exception
	 */
	private Integer searchSimultaniousUser(String[] splitActivationKeyList) throws ParseException {

		int index = 0;
		for (String s : splitActivationKeyList) {
			index++;
			if (s.contains(CommonConstants.SIMULTANIOUS_USER)) {
				break;
			}
		}
		Integer sumultaniousUser =
				Integer.parseInt(splitActivationKeyList[index - 1].split(":")[1].trim());
		return sumultaniousUser;
	}

	/**
	 * Gets the mac adress from licence.
	 *
	 * @param splitActivationKeyList the split activation key list
	 * @return the mac adress from licence
	 * @throws ParseException the parse exception
	 */
	private String getMacAdressFromLicence(String[] splitActivationKeyList) throws ParseException {

		int index = 0;
		for (String s : splitActivationKeyList) {
			index++;
			if (s.contains(CommonConstants.MAC)) {
				break;
			}
		}

		return splitActivationKeyList[index - 1].split(":")[1].trim();
	}

	/**
	 * Gets the mac adress from server.
	 *
	 * @return the mac adress from server
	 * @throws ParseException the parse exception
	 */
	@Override
	public String getMacAdressFromServer() throws ParseException {

		List<String> lstConf = habilitationRepository.findMacAdress();
		return lstConf.stream().filter(item -> (!ACMValidationUtils.isNullOrEmpty(item)
				&& (item.contains("Adresse physique") || item.contains("Physical address"))))
				.collect(Collectors.toList()).get(0).split(":")[1].trim();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.HabilitationService#find(com.acm.utils.dtos.HabilitationDTO)
	 */
	@Override
	public List<HabilitationDTO> find(HabilitationDTO habilitationDTO) {

		Preconditions.checkNotNull(habilitationDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// init QHabilitation
		QHabilitation qHabilitation = QHabilitation.habilitation;

		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find only enabled data
		predicate.and(qHabilitation.enabled.eq(Boolean.TRUE));

		// find by client
		predicate.and(qHabilitation.client.eq(CommonConstants.APP_NAME));

		// find by groupe id
		if (!ACMValidationUtils.isNullOrEmpty(habilitationDTO.getIdGroupe())) {
			predicate.and(qHabilitation.idGroupe.eq(habilitationDTO.getIdGroupe()));
		}

		// find by code habilitation / Code-IHMRoute
		if (!ACMValidationUtils.isNullOrEmpty(habilitationDTO.getAcmHabilitation())) {
			predicate.and(qHabilitation.acmHabilitation.eq(habilitationDTO.getAcmHabilitation()));
		}

		// find by IHMRoute
		if (!ACMValidationUtils.isNullOrEmpty(habilitationDTO.getAcmWebRoute())) {
			predicate.and(qHabilitation.acmWebRoute.eq(habilitationDTO.getAcmWebRoute()));
		}

		// QueryDSL using springDATA
		Iterable<Habilitation> iterable = habilitationRepository.findAll(predicate);
		List<Habilitation> habilitations = new ArrayList<>();
		iterable.forEach(habilitations::add);
		logger.info("{} : habilitations was founded", habilitations.size());

		// mapping returned list
		List<HabilitationDTO> habilitationDTOs = new ArrayList<>();
		habilitations.forEach(habilitation -> habilitationDTOs
				.add(mapper.map(habilitation, HabilitationDTO.class)));
		return habilitationDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.HabilitationService#find(com.acm.utils.dtos.HabilitationDTO)
	 */
	@Override
	public List<HabilitationDTO> findAll(HabilitationDTO habilitationDTO) {

		Preconditions.checkNotNull(habilitationDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// init QHabilitation
		QHabilitation qHabilitation = QHabilitation.habilitation;

		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find by client
		predicate.and(qHabilitation.client.eq(CommonConstants.APP_NAME));

		// find by groupe id
		if (!ACMValidationUtils.isNullOrEmpty(habilitationDTO.getIdGroupe())) {
			predicate.and(qHabilitation.idGroupe.eq(habilitationDTO.getIdGroupe()));
		}

		// find by code habilitation / Code-IHMRoute
		if (!ACMValidationUtils.isNullOrEmpty(habilitationDTO.getAcmHabilitation())) {
			predicate.and(qHabilitation.acmHabilitation.eq(habilitationDTO.getAcmHabilitation()));
		}

		// find by IHMRoute
		if (!ACMValidationUtils.isNullOrEmpty(habilitationDTO.getAcmWebRoute())) {
			predicate.and(qHabilitation.acmWebRoute.eq(habilitationDTO.getAcmWebRoute()));
		}

		// QueryDSL using springDATA
		Iterable<Habilitation> iterable = habilitationRepository.findAll(predicate);
		List<Habilitation> habilitations = new ArrayList<>();
		iterable.forEach(habilitations::add);
		logger.info("{} : habilitations was founded", habilitations.size());

		// mapping returned list
		List<HabilitationDTO> habilitationDTOs = new ArrayList<>();
		habilitations.forEach(habilitation -> habilitationDTOs
				.add(mapper.map(habilitation, HabilitationDTO.class)));
		return habilitationDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.HabilitationService#save(com.acm.utils.dtos.HabilitationDTO)
	 */
	@Override
	public HabilitationDTO save(HabilitationDTO habilitationDTO) {

		Preconditions.checkNotNull(habilitationDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Habilitation habilitation = mapper.map(habilitationDTO, Habilitation.class);
		CommonFunctions.mapperToUpdateWithEnabled(habilitation, userClient, logger);
		Habilitation newHabilitation = habilitationRepository.save(habilitation);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, Habilitation.class.getSimpleName());
		return mapper.map(newHabilitation, HabilitationDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.HabilitationService#findByGroupeAndEnable()
	 */
	@Override
	public List<HabilitationDTO> findByGroupeID() throws ResourcesNotFoundException {

		List<Habilitation> habilitations = new ArrayList<>();
		// find DEFAULT groupe ID by CODE = ACM_DEFAULT_GROUP
		GroupeDTO defaultGroup = groupeService.findByCode(CommonConstants.DEFAULT_GROUPE);

		// find list habilitations by ID default group
		habilitations.addAll(habilitationRepository.findByIdGroupe(defaultGroup.getId()));

		// mapping data
		List<HabilitationDTO> habilitationDTOs = new ArrayList<>();
		if (!habilitations.isEmpty()) {
			habilitations.forEach(habilitation -> {
				HabilitationDTO habilitationDTO = mapper.map(habilitation, HabilitationDTO.class);
				if (!ACMValidationUtils.isNullOrEmpty(habilitation.getHabilitationIHMRoute())) {
					HabilitationIHMRouteDTO habilitationIHMRouteDTO = mapper.map(
							habilitation.getHabilitationIHMRoute(), HabilitationIHMRouteDTO.class);
					habilitationDTO.setHabilitationIHMRouteDTO(habilitationIHMRouteDTO);
				}
				habilitationDTOs.add(habilitationDTO);
			});

		}
		logger.info("{} : habilitations was founded for groupe : ACM_DEFAULT_GROUP.",
				habilitationDTOs.size());
		return habilitationDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.HabilitationService#saveAll(com.acm.utils.dtos.HabilitationDTO)
	 */
	@Override
	public void saveAll(List<HabilitationDTO> habilitationDTOs) {

		for (HabilitationDTO habilitationDTO : habilitationDTOs) {
			Habilitation habilitation = mapper.map(habilitationDTO, Habilitation.class);
			if (!ACMValidationUtils.isNullOrEmpty(habilitationDTO.getHabilitationIHMRouteDTO())) {
				habilitation.setHabilitationIHMRoute(mapper.map(
						habilitationDTO.getHabilitationIHMRouteDTO(), HabilitationIHMRoute.class));
			}
			Habilitation newHabilitation = habilitationRepository.save(habilitation);
			logger.debug("{}", newHabilitation);
			logger.info(CommonLoggerMessage.SUCCESFULL_CREATE,
					Habilitation.class.getSimpleName() + " with ID : " + newHabilitation.getId());
		}
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.HabilitationService#updateAll(com.acm.utils.dtos.HabilitationDTO)
	 */
	@Override
	public List<HabilitationDTO> updateAll(List<HabilitationDTO> habilitationDTOs)
			throws ResourcesNotFoundException {

		List<HabilitationDTO> updatedData = new ArrayList<>();
		for (HabilitationDTO habilitationDTO : habilitationDTOs) {
			Preconditions.checkNotNull(habilitationDTO,
					CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
			Preconditions.checkNotNull(habilitationDTO.getId(),
					CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
			logger.info("Update Habilitation with ID = {}", habilitationDTO.getId());
			Habilitation oldHabilitation =
					habilitationRepository.findById(habilitationDTO.getId()).orElse(null);
			// check if object is null
			if (oldHabilitation == null) {
				logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
						Habilitation.class.getSimpleName());
				throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
						CommonExceptionsMessage.NOT_FOUND + Habilitation.class.getSimpleName()
								+ CommonExceptionsMessage.WITH_ID + habilitationDTO.getId());
			}

			// mapping new data with existing data (oldProduct)
			mapper.map(habilitationDTO, oldHabilitation);
			// update & persist data in DB
			CommonFunctions.mapperToUpdateWithEnabled(oldHabilitation, userClient, logger);
			// updating data
			Habilitation newHabilitation = habilitationRepository.save(oldHabilitation);
			logger.debug("{}", newHabilitation);
			logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, Habilitation.class.getSimpleName());
			updatedData.add(mapper.map(newHabilitation, HabilitationDTO.class));
		}
		return updatedData;
	}
}
