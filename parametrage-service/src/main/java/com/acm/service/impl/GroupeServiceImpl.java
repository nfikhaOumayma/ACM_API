/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import com.acm.aop.history.ProcessHistorySetting;
import com.acm.client.UserClient;
import com.acm.constants.common.CommonAOPConstants;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.GroupeUsersFoundException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.GroupeRepository;
import com.acm.service.GroupeService;
import com.acm.service.HabilitationService;
import com.acm.service.SettingHistoriqueService;
import com.acm.utils.dtos.GroupeDTO;
import com.acm.utils.dtos.HabilitationDTO;
import com.acm.utils.dtos.SettingHistoriqueDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.dtos.pagination.GroupePaginationDTO;
import com.acm.utils.models.Groupe;
import com.acm.utils.models.Product;
import com.acm.utils.models.QGroupe;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link GroupeServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 0.3.0
 */
@Service
public class GroupeServiceImpl implements GroupeService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(GroupeServiceImpl.class);

	/** The groupe repository. */
	@Autowired
	private GroupeRepository groupeRepository;

	/** The setting historique service. */
	@Autowired
	private SettingHistoriqueService settingHistoriqueService;

	/** The user client. */
	@Autowired
	private UserClient userClient;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The habilitation service. */
	@Autowired
	private HabilitationService habilitationService;

	/** The Environment. */
	@Autowired
	private Environment environment;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.GroupeService#find(com.acm.utils.dtos.GroupeDTO)
	 */
	@Override
	public List<GroupeDTO> find(GroupeDTO groupeDTO) {

		// init QGroupe
		QGroupe qGroupe = QGroupe.groupe;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find only enabled data
		predicate.and(qGroupe.enabled.eq(Boolean.TRUE));

		// find by ID
		if (!ACMValidationUtils.isNullOrEmpty(groupeDTO.getId())) {
			predicate.and(qGroupe.id.eq(groupeDTO.getId()));
		}

		// find by CODE
		if (!ACMValidationUtils.isNullOrEmpty(groupeDTO.getCode())) {
			predicate.and(qGroupe.code.eq(groupeDTO.getCode()));
		}

		// find by ID USER PROFILE Extern
		if (!ACMValidationUtils.isNullOrEmpty(groupeDTO.getUserProfileIDExtern())) {
			predicate.and(qGroupe.userProfileIDExtern.eq(groupeDTO.getUserProfileIDExtern()));
		}

		// QueryDSL using springDATA
		Iterable<Groupe> iterable = groupeRepository.findAll(predicate);
		List<Groupe> groupes = new ArrayList<>();
		iterable.forEach(groupes::add);
		logger.info("{} : GROUPE was founded", groupes.size());

		// mapping returned list
		List<GroupeDTO> groupeDTOs = new ArrayList<>();
		groupes.forEach(groupe -> groupeDTOs.add(mapper.map(groupe, GroupeDTO.class)));

		logger.info("Returning founded data ...");
		return groupeDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.GroupeService#save(java.lang.Long, com.acm.utils.dtos.GroupeDTO)
	 */
	@Override
	public GroupeDTO save(Long id, GroupeDTO groupeDTO) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(groupeDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.info("Update groupe  with ID = {}", id);
		Groupe oldGroupe = groupeRepository.findById(id).orElse(null);
		// check if object is null
		if (oldGroupe == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Groupe.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + Groupe.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		// init settingHistory object
		SettingHistoriqueDTO settingHistoriqueDTO = new SettingHistoriqueDTO(
				CommonFunctions.getTableNameFromClass(Groupe.class), CommonAOPConstants.UPDATE, id,
				CommonFunctions.convertObjectToJSONString(oldGroupe));

		// mapping new data with existing data (oldGroupe)
		oldGroupe.setLibelle(groupeDTO.getLibelle());
		oldGroupe.setDescription(groupeDTO.getDescription());
		CommonFunctions.mapperToUpdate(oldGroupe, userClient, logger);
		Groupe newGroupe = groupeRepository.save(oldGroupe);
		GroupeDTO newGroupeDTO = mapper.map(newGroupe, GroupeDTO.class);

		// saving history setting
		settingHistoriqueDTO.setUpdatedBy(newGroupe.getUpdatedBy());
		settingHistoriqueDTO.setNewData(CommonFunctions.convertObjectToJSONString(newGroupeDTO));
		settingHistoriqueService.save(settingHistoriqueDTO);
		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, Groupe.class.getSimpleName());
		return newGroupeDTO;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.GroupeService#updateEnabled(com.acm.utils.dtos.GroupeDTO)
	 */
	@Override
	public GroupeDTO updateEnabled(GroupeDTO groupeDTO)
			throws ResourcesNotFoundException, GroupeUsersFoundException {

		Preconditions.checkNotNull(groupeDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Preconditions.checkNotNull(groupeDTO.getId(), CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Update groupe  with ID = {}", groupeDTO.getId());
		Groupe oldGroupe = groupeRepository.findById(groupeDTO.getId()).orElse(null);
		// check if object is null
		if (oldGroupe == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Groupe.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + Groupe.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + groupeDTO.getId());
		}

		// init settingHistory object
		SettingHistoriqueDTO settingHistoriqueDTO = new SettingHistoriqueDTO(
				CommonFunctions.getTableNameFromClass(Groupe.class), CommonAOPConstants.UPDATE,
				groupeDTO.getId(), CommonFunctions.convertObjectToJSONString(oldGroupe));

		// mapping new data with existing data (oldGroupe)
		if ((!groupeDTO.getEnabled()) && !oldGroupe.getUsers().isEmpty()) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Groupe.class.getSimpleName());
			throw new GroupeUsersFoundException();
		}
		else {
			// mapping new data with existing data (oldGroupe)
			oldGroupe.setEnabled(groupeDTO.getEnabled());
			CommonFunctions.mapperToUpdateWithEnabled(oldGroupe, userClient, logger);
			Groupe newGroupe = groupeRepository.save(oldGroupe);
			GroupeDTO newGroupeDTO = mapper.map(newGroupe, GroupeDTO.class);

			// saving history setting
			settingHistoriqueDTO.setUpdatedBy(newGroupe.getUpdatedBy());
			settingHistoriqueDTO
					.setNewData(CommonFunctions.convertObjectToJSONString(newGroupeDTO));
			settingHistoriqueService.save(settingHistoriqueDTO);
			logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, Groupe.class.getSimpleName());
			return newGroupeDTO;
		}
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.GroupeService#save(com.acm.utils.dtos.GroupeDTO)
	 */
	@Override
	@ProcessHistorySetting(action = CommonAOPConstants.NEW)
	public GroupeDTO save(GroupeDTO groupeDTO) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(groupeDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		groupeDTO.setCode(groupeDTO.getCode().toUpperCase().replace(" ", "_"));
		Groupe groupe = mapper.map(groupeDTO, Groupe.class);
		CommonFunctions.mapperToSave(groupe, userClient, logger);
		Groupe newgroupe = groupeRepository.save(groupe);

		// Add habilitation of groupe default with disabled
		List<HabilitationDTO> habilitationDTOs = habilitationService.findByGroupeID();
		List<HabilitationDTO> newHabilitationDTOs = new ArrayList<>();
		habilitationDTOs.forEach(habilitation -> {
			HabilitationDTO habilitationDTO = new HabilitationDTO(habilitation.getActions(),
					habilitation.getClient(), habilitation.getAcmHabilitation(),
					habilitation.getAcmWebRoute(), newgroupe.getId(), habilitation.getValue(),
					habilitation.getEnabled(), habilitation.getDescription());

			if (!ACMValidationUtils.isNullOrEmpty(habilitation.getHabilitationIHMRouteDTO())) {
				habilitationDTO
						.setHabilitationIHMRouteDTO(habilitation.getHabilitationIHMRouteDTO());
			}
			newHabilitationDTOs.add(habilitationDTO);
		});
		// saving newHabilitationDTOs for groupe
		habilitationService.saveAll(newHabilitationDTOs);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, Groupe.class.getSimpleName());
		return mapper.map(newgroupe, GroupeDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.GroupeService#find()
	 */
	@Override
	public List<GroupeDTO> find() {

		List<Groupe> groupes = groupeRepository.findAll();
		// mapping returned list
		List<GroupeDTO> groupeDTOs = new ArrayList<>();
		groupes.forEach(groupe -> groupeDTOs.add(mapper.map(groupe, GroupeDTO.class)));

		logger.info("Returning founded data ...");
		return groupeDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.GroupeService#find(com.acm.utils.dtos.pagination.groupePaginationDTO)
	 */
	@Override
	public GroupePaginationDTO find(GroupePaginationDTO groupePaginationDTO) {

		Preconditions.checkNotNull(groupePaginationDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// init default PageNumber if NULL : 0 (first page)
		if (ACMValidationUtils.isNullOrEmpty(groupePaginationDTO.getPageNumber())) {
			groupePaginationDTO.setPageNumber(0);
		}
		// init default PageSize if NULL : 10 elements
		if (ACMValidationUtils.isNullOrEmpty(groupePaginationDTO.getPageSize())) {
			groupePaginationDTO.setPageSize(10);
		}
		// setting default data
		groupePaginationDTO.setResultsGroupes(new ArrayList<>());
		// setting default totals pages
		groupePaginationDTO.setTotalElements(0L);
		// setting default totals elements
		groupePaginationDTO.setTotalPages(0);
		// init QGroupe
		QGroupe qGroupe = QGroupe.groupe;
		// build Predicate using given params
		BooleanBuilder predicate = buildQuery(groupePaginationDTO.getParams(), qGroupe);

		// init pageable params (page number / page size / sorting direction if exist)
		Pageable pageable = null;
		if ("1".equals(groupePaginationDTO.getSortDirection())
				&& !ACMValidationUtils.isNullOrEmpty(groupePaginationDTO.getSortField())) {
			pageable = PageRequest.of(groupePaginationDTO.getPageNumber(),
					groupePaginationDTO.getPageSize(), Sort.Direction.ASC,
					groupePaginationDTO.getSortField());
		}
		else if ("-1".equals(groupePaginationDTO.getSortDirection())
				&& !ACMValidationUtils.isNullOrEmpty(groupePaginationDTO.getSortField())) {
			pageable = PageRequest.of(groupePaginationDTO.getPageNumber(),
					groupePaginationDTO.getPageSize(), Sort.Direction.DESC,
					groupePaginationDTO.getSortField());
		}
		else {
			// default sort by code : ASC
			pageable = PageRequest.of(groupePaginationDTO.getPageNumber(),
					groupePaginationDTO.getPageSize(), Sort.Direction.ASC, "code");
		}

		// load data
		Page<Groupe> pagedResult = groupeRepository.findAll(predicate, pageable);
		if (pagedResult.hasContent()) {
			List<Groupe> groupes = pagedResult.getContent();
			logger.info("{} : Groupe was founded (PageNumber = {} / PageSize = {} )",
					groupes.size(), groupePaginationDTO.getPageNumber(),
					groupePaginationDTO.getPageSize());
			List<GroupeDTO> groupeDTOs = new ArrayList<>();
			groupes.forEach(groupe -> groupeDTOs.add(mapper.map(groupe, GroupeDTO.class)));
			// setting data
			groupePaginationDTO.setResultsGroupes(groupeDTOs);
			// setting totals pages
			groupePaginationDTO.setTotalElements(pagedResult.getTotalElements());
			// setting totals elements
			groupePaginationDTO.setTotalPages(pagedResult.getTotalPages());
		}
		return groupePaginationDTO;
	}

	/**
	 * Builds the query.
	 *
	 * @author MoezMhiri
	 * @param groupeDTO the groupe DTO
	 * @param qGroupe the q groupe
	 * @return the boolean builder
	 */
	private BooleanBuilder buildQuery(GroupeDTO groupeDTO, QGroupe qGroupe) {

		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();
		// loanParams.code
		if (!ACMValidationUtils.isNullOrEmpty(groupeDTO.getCode())) {
			predicate.and(qGroupe.code.like("%" + groupeDTO.getCode() + "%"));
		}

		// loanParams.groupeLibelle
		if (!ACMValidationUtils.isNullOrEmpty(groupeDTO.getLibelle())) {
			predicate.and(qGroupe.libelle.like("%" + groupeDTO.getLibelle() + "%"));
		}

		// loanParams.groupeDescription
		if (!ACMValidationUtils.isNullOrEmpty(groupeDTO.getDescription())) {
			predicate.and(qGroupe.description.like("%" + groupeDTO.getDescription() + "%"));
		}
		// loanParams.enabled
		if (!ACMValidationUtils.isNullOrEmpty(groupeDTO.getEnabled())) {
			predicate.and(qGroupe.enabled.eq(groupeDTO.getEnabled()));
		}
		return predicate;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.GroupeService#find(java.lang.String)
	 */
	@Override
	public GroupeDTO findByCode(String code) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(code, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find Product by ID : {}", code);
		// init groupDTO
		GroupeDTO groupeDTO = new GroupeDTO();
		// find group by code in parameter
		Groupe groupe = groupeRepository.findByCode(code);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(groupe)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Groupe.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty("exception.message.not.found")
							+ Product.class.getSimpleName() + CommonExceptionsMessage.WITH_ID
							+ code);
		}
		else {
			groupeDTO = mapper.map(groupe, GroupeDTO.class);
			// find users of group found
			UserDTO userDTO = new UserDTO();
			userDTO.setGroupeCode(groupe.getCode());
			List<UserDTO> userDTOs = userClient.findByGroupe(userDTO);
			groupeDTO.setUserDTOs(userDTOs);
		}
		return groupeDTO;
	}
}
