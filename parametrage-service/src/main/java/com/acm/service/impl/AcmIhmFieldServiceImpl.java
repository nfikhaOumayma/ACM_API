/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.util.ArrayList;
import java.util.HashSet;
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
import com.acm.repository.AcmIhmFieldRepository;
import com.acm.service.AcmIhmFieldGroupeService;
import com.acm.service.AcmIhmFieldService;
import com.acm.service.AcmIhmValidatorService;
import com.acm.utils.dtos.AcmIhmFieldDTO;
import com.acm.utils.dtos.AcmIhmValidatorDTO;
import com.acm.utils.dtos.GroupeDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.models.AcmIhmField;
import com.acm.utils.models.AcmIhmForm;
import com.acm.utils.models.AcmIhmValidator;
import com.acm.utils.models.QAcmIhmField;
import com.acm.utils.models.SettingRequiredStep;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link AcmIhmFieldServiceImpl} class.
 *
 * @author ManelLamloum
 * @since 1.0.14
 */
@Service
public class AcmIhmFieldServiceImpl implements AcmIhmFieldService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(AcmIhmFieldServiceImpl.class);

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The acm ihm field repository. */
	@Autowired
	private AcmIhmFieldRepository acmIhmFieldRepository;

	/** The user Client client. */
	@Autowired
	private UserClient userClient;

	/** The acm ihm field groupe service. */
	@Autowired
	AcmIhmFieldGroupeService acmIhmFieldGroupeService;

	/** The acm ihm validator service. */
	@Autowired
	AcmIhmValidatorService acmIhmValidatorService;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmIhmFieldService#find(com.acm.utils.dtos.AcmIhmFieldDTO)
	 */
	@Override
	public List<AcmIhmFieldDTO> find(AcmIhmFieldDTO acmIhmFieldDTO) {

		Preconditions.checkNotNull(acmIhmFieldDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		if (ACMValidationUtils.isNullOrEmpty(acmIhmFieldDTO.getCodeField())
				&& ACMValidationUtils.isNullOrEmpty(acmIhmFieldDTO.getId())
				&& ACMValidationUtils.isNullOrEmpty(acmIhmFieldDTO.getCodeForm())) {
			return new ArrayList<>();
		}
		// init QHabilitation
		QAcmIhmField qAcmIhmField = QAcmIhmField.acmIhmField;

		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find enabled data
		if (!ACMValidationUtils.isNullOrEmpty(acmIhmFieldDTO.getEnabled())) {
			predicate.and(qAcmIhmField.enabled.eq(Boolean.TRUE));
		}
		// find by fieldCode
		if (!ACMValidationUtils.isNullOrEmpty(acmIhmFieldDTO.getCodeField())) {
			predicate.and(qAcmIhmField.codeField.eq(acmIhmFieldDTO.getCodeField()));
		}
		// find by id
		if (!ACMValidationUtils.isNullOrEmpty(acmIhmFieldDTO.getId())) {
			predicate.and(qAcmIhmField.id.eq(acmIhmFieldDTO.getId()));
		}
		// find by formCode
		if (!ACMValidationUtils.isNullOrEmpty(acmIhmFieldDTO.getCodeForm())) {
			predicate.and(qAcmIhmField.acmIhmForm.codePage.eq(acmIhmFieldDTO.getCodeForm()));
		}
		final GroupeDTO groupeDTO;
		// if we need the habilitation for connected user :
		if (ACMValidationUtils.isNullOrEmpty(acmIhmFieldDTO.getCodeUserGroup())) {
			// find connected user details
			UserDTO connectedUser = CommonFunctions.getConnectedUser(logger);
			// get groupe of user
			groupeDTO = connectedUser.getGroupes().iterator().next();
		}
		// else if we need the habilitation for a specefic group of user
		else {
			groupeDTO = new GroupeDTO();
			groupeDTO.setCode(acmIhmFieldDTO.getCodeUserGroup());
		}
		// QueryDSL using springDATA
		Iterable<AcmIhmField> iterable = acmIhmFieldRepository.findAll(predicate);
		List<AcmIhmField> acmIhmFields = new ArrayList<>();
		iterable.forEach(acmIhmFields::add);
		logger.info("{} : acmIhmFields was founded", acmIhmFields.size());

		// mapping returned list && fill habilitation attribut in each Field
		List<AcmIhmFieldDTO> acmIhmFieldDTOs = new ArrayList<>();
		acmIhmFields.forEach(acmIhmField -> {
			AcmIhmFieldDTO acmIhmFieldDTOE = mapper.map(acmIhmField, AcmIhmFieldDTO.class);
			// setting field Habilitation foreach field
			if (!ACMValidationUtils.isNullOrEmpty(acmIhmField.getAcmIhmFieldGroupes())) {
				// get IhmGroupField of Group of connected user and current acmIhmField
				String action = acmIhmField.getAcmIhmFieldGroupes().stream()
						.filter(ifg -> ifg.getGroup().getCode().equals(groupeDTO.getCode()))
						.findFirst().map(ifg2 -> ifg2.getHabilitation()).orElse(null);
				acmIhmFieldDTOE.setHabilitation(action);
			}
			acmIhmFieldDTOs.add(acmIhmFieldDTOE);
		});

		logger.info("Returning founded data ...");
		return acmIhmFieldDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmIhmFieldService#save(java.lang.Long,
	 * com.acm.utils.dtos.AcmIhmFieldDTO)
	 */
	@Override
	public AcmIhmFieldDTO save(Long id, AcmIhmFieldDTO acmIhmFieldDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(acmIhmFieldDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.debug("Update acmIhmFieldDTO  with ID = {}", id);
		AcmIhmField oldAcmIhmField = acmIhmFieldRepository.findById(id).orElse(null);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(oldAcmIhmField)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, AcmIhmField.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + AcmIhmField.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}

		// set oldAcmIhmField with new data
		oldAcmIhmField.setTitre(acmIhmFieldDTO.getTitre());
		oldAcmIhmField.setDescription(acmIhmFieldDTO.getDescription());
		oldAcmIhmField.setTypeField(acmIhmFieldDTO.getTypeField());
		oldAcmIhmField.setEnabled(acmIhmFieldDTO.getEnabled());
		oldAcmIhmField.setFormControlName(acmIhmFieldDTO.getFormControlName());
		oldAcmIhmField.setOrdre(acmIhmFieldDTO.getOrdre());
		oldAcmIhmField.setPlaceholder(acmIhmFieldDTO.getPlaceholder());

		// set the new validators list of this field
		oldAcmIhmField.setValidators(new HashSet<>());
		acmIhmFieldDTO.getValidators().forEach(validatorDTO -> {
			oldAcmIhmField.getValidators().add(mapper.map(validatorDTO, AcmIhmValidator.class));
		});

		CommonFunctions.mapperToUpdate(oldAcmIhmField, userClient, logger);
		// update field
		AcmIhmField newAcmIhmField = acmIhmFieldRepository.save(oldAcmIhmField);

		logger.debug(CommonLoggerMessage.SUCCESFULL_UPDATE, AcmIhmField.class.getSimpleName());
		return mapper.map(newAcmIhmField, AcmIhmFieldDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmIhmFieldService#saveAll(java.util.List)
	 */
	@Override
	public List<AcmIhmFieldDTO> saveAll(List<AcmIhmFieldDTO> acmIhmFieldDTOs) {

		Preconditions.checkNotNull(acmIhmFieldDTOs, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		List<AcmIhmFieldDTO> newAcmIhmFieldDTOs = new ArrayList<>();

		for (AcmIhmFieldDTO acmIhmFieldDTO : acmIhmFieldDTOs) {
			Preconditions.checkNotNull(acmIhmFieldDTO,
					CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
			AcmIhmField acmIhmField = mapper.map(acmIhmFieldDTO, AcmIhmField.class);
			acmIhmField
					.setAcmIhmForm(mapper.map(acmIhmFieldDTO.getAcmIhmFormDTO(), AcmIhmForm.class));

			acmIhmField.setValidators(new HashSet<>());
			for (AcmIhmValidatorDTO acmIhmValidatorDTO : acmIhmFieldDTO.getValidators()) {
				acmIhmField.getValidators()
						.add(mapper.map(acmIhmValidatorDTO, AcmIhmValidator.class));
			}

			CommonFunctions.mapperToSave(acmIhmField, userClient, logger);
			AcmIhmField newAcmIhmField = acmIhmFieldRepository.save(acmIhmField);

			logger.info(CommonLoggerMessage.SUCCESFULL_CREATE,
					SettingRequiredStep.class.getSimpleName());
			newAcmIhmFieldDTOs.add(mapper.map(newAcmIhmField, AcmIhmFieldDTO.class));
		}

		return newAcmIhmFieldDTOs;
	}
}
