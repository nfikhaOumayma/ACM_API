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
import org.springframework.stereotype.Service;

import com.acm.client.UserClient;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.AcmIhmFormRepository;
import com.acm.service.AcmIhmFieldGroupeService;
import com.acm.service.AcmIhmFormService;
import com.acm.utils.dtos.AcmIhmFieldDTO;
import com.acm.utils.dtos.AcmIhmFormDTO;
import com.acm.utils.dtos.GroupeDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.models.AcmIhmField;
import com.acm.utils.models.AcmIhmForm;
import com.acm.utils.models.Groupe;
import com.acm.utils.models.HabilitationIHMRoute;
import com.acm.utils.models.QAcmIhmForm;
import com.acm.utils.models.SettingRequiredStep;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link AcmIhmFormServiceImpl} class.
 *
 * @author ManelLamloum
 * @since 1.0.14
 */
@Service
public class AcmIhmFormServiceImpl implements AcmIhmFormService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(AcmIhmFormServiceImpl.class);

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The acm ihm field repository. */
	@Autowired
	private AcmIhmFormRepository acmIhmFormRepository;

	/** The user Client client. */
	@Autowired
	private UserClient userClient;

	/** The acm ihm field groupe service. */
	@Autowired
	AcmIhmFieldGroupeService acmIhmFieldGroupeService;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmIhmFormService#find(com.acm.utils.dtos.AcmIhmFormDTO)
	 */
	@Override
	public List<AcmIhmFormDTO> find(AcmIhmFormDTO acmIhmFormDTO) {

		Preconditions.checkNotNull(acmIhmFormDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);

		// checking data
		if (ACMValidationUtils.isNullOrEmpty(acmIhmFormDTO.getCodePage())
				&& ACMValidationUtils.isNullOrEmpty(acmIhmFormDTO.getHabilitationIHMRouteDTO())
				&& ACMValidationUtils
						.isNullOrEmpty(acmIhmFormDTO.getHabilitationIHMRouteDTO().getId())) {
			return new ArrayList<>();
		}
		// init QHabilitation
		QAcmIhmForm qAcmIhmForm = QAcmIhmForm.acmIhmForm;

		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find only enabled data
		predicate.and(qAcmIhmForm.enabled.eq(Boolean.TRUE));

		// find by ihmRoute code
		if (!ACMValidationUtils.isNullOrEmpty(acmIhmFormDTO.getHabilitationIHMRouteDTO())
				&& !ACMValidationUtils.isNullOrEmpty(
						acmIhmFormDTO.getHabilitationIHMRouteDTO().getCodeIHMRoute())) {
			predicate.and(qAcmIhmForm.habilitationIHMRoute.codeIHMRoute
					.eq(acmIhmFormDTO.getHabilitationIHMRouteDTO().getCodeIHMRoute()));
		}
		// find by ihmRoute
		if (!ACMValidationUtils.isNullOrEmpty(acmIhmFormDTO.getHabilitationIHMRouteDTO())
				&& !ACMValidationUtils
						.isNullOrEmpty(acmIhmFormDTO.getHabilitationIHMRouteDTO().getId())) {
			predicate.and(qAcmIhmForm.habilitationIHMRoute.id
					.eq(acmIhmFormDTO.getHabilitationIHMRouteDTO().getId()));
		}

		// find by codePage
		if (!ACMValidationUtils.isNullOrEmpty(acmIhmFormDTO.getCodePage())) {
			predicate.and(qAcmIhmForm.codePage.eq(acmIhmFormDTO.getCodePage()));
		}

		/*** Begin get current group DTO **/
		// find connected user details
		UserDTO connectedUser = CommonFunctions.getConnectedUser(logger);
		// get groupe of user
		GroupeDTO groupeDTO = connectedUser.getGroupes().iterator().next();
		if (ACMValidationUtils.isNullOrEmpty(groupeDTO)) {
			logger.info(CommonLoggerMessage.NOT_FOUND_OBJECT, Groupe.class.getSimpleName());
		}
		/*** End get current group DTO **/
		// get the resulted list
		// QueryDSL using springDATA
		Iterable<AcmIhmForm> iterable = acmIhmFormRepository.findAll(predicate);
		List<AcmIhmForm> acmIhmForms = new ArrayList<>();
		iterable.forEach(acmIhmForms::add);
		logger.info("{} : acmIhmForms was founded", acmIhmForms.size());

		// mapping returned list
		List<AcmIhmFormDTO> acmIhmFormDTOs = new ArrayList<>();
		acmIhmForms.forEach(acmIhmForm -> {
			// mapping data
			AcmIhmFormDTO acmIhmFormDTOE = mapper.map(acmIhmForm, AcmIhmFormDTO.class);
			// mapping returned list && fill habilitation in each Field

			if (acmIhmFormDTO.getNeedFields().equals(Boolean.TRUE)) {
				List<AcmIhmFieldDTO> acmIhmFieldDTOs = new ArrayList<>();
				acmIhmForm.getAcmIhmFields().forEach(acmIhmField -> {
					// get only enabled fields
					if (acmIhmField.getEnabled().equals(Boolean.TRUE)) {
						AcmIhmFieldDTO acmIhmFieldDTOE =
								mapper.map(acmIhmField, AcmIhmFieldDTO.class);

						// setting field Habilitation foreach field
						if (!ACMValidationUtils
								.isNullOrEmpty(acmIhmField.getAcmIhmFieldGroupes())) {
							// get IhmGroupField of Group of connected user and current acmIhmField
							String action = acmIhmField.getAcmIhmFieldGroupes().stream()
									.filter(ifg -> ifg.getGroup().getCode()
											.equals(groupeDTO.getCode()))
									.findFirst().map(ifg2 -> ifg2.getHabilitation()).orElse(null);
							acmIhmFieldDTOE.setHabilitation(action);
						}

						acmIhmFieldDTOs.add(acmIhmFieldDTOE);
					}
				});
				// setting data
				acmIhmFormDTOE.setAcmIhmFields(acmIhmFieldDTOs);

			}
			// add data
			acmIhmFormDTOs.add(acmIhmFormDTOE);
		});
		logger.info("Returning founded data ...");
		return acmIhmFormDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmIhmFormService#save(com.acm.utils.dtos.AcmIhmFormDTO)
	 */
	@Override
	public AcmIhmFormDTO save(AcmIhmFormDTO acmIhmFormDTO) {

		Preconditions.checkNotNull(acmIhmFormDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		AcmIhmForm acmIhmForm = mapper.map(acmIhmFormDTO, AcmIhmForm.class);
		acmIhmForm.setHabilitationIHMRoute(
				mapper.map(acmIhmFormDTO.getHabilitationIHMRouteDTO(), HabilitationIHMRoute.class));

		CommonFunctions.mapperToSave(acmIhmForm, userClient, logger);
		AcmIhmForm newAcmIhmForm = acmIhmFormRepository.save(acmIhmForm);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE,
				SettingRequiredStep.class.getSimpleName());
		return mapper.map(newAcmIhmForm, AcmIhmFormDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmIhmFormService#save(com.acm.utils.dtos.AcmIhmFormDTO, java.lang.Long)
	 */
	@Override
	public AcmIhmFormDTO save(AcmIhmFormDTO acmIhmFormDTO, Long id)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(acmIhmFormDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.debug("Update acmIhmFieldDTO  with ID = {}", id);
		AcmIhmForm oldAcmIhmForm = acmIhmFormRepository.findById(id).orElse(null);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(oldAcmIhmForm)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, AcmIhmField.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + AcmIhmField.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}

		// set oldAcmIhmField with new data
		oldAcmIhmForm.setCodePage(acmIhmFormDTO.getCodePage());
		oldAcmIhmForm.setDescription(acmIhmFormDTO.getDescription());

		CommonFunctions.mapperToUpdate(oldAcmIhmForm, userClient, logger);
		// update field
		AcmIhmForm newAcmIhmForm = acmIhmFormRepository.save(oldAcmIhmForm);

		logger.debug(CommonLoggerMessage.SUCCESFULL_UPDATE, AcmIhmField.class.getSimpleName());
		return mapper.map(newAcmIhmForm, AcmIhmFormDTO.class);

	}

}
