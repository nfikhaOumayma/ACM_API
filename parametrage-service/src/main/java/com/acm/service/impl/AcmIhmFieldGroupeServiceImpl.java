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
import com.acm.repository.AcmIhmFieldGroupeRepository;
import com.acm.service.AcmIhmFieldGroupeService;
import com.acm.utils.dtos.AcmIhmFieldGroupeDTO;
import com.acm.utils.models.AcmIhmFieldGroupe;
import com.acm.utils.models.QAcmIhmFieldGroupe;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link AcmIhmFieldGroupeServiceImpl} class.
 *
 * @author ManelLamloum
 * @since 1.0.14
 */
@Service
public class AcmIhmFieldGroupeServiceImpl implements AcmIhmFieldGroupeService {

	/** The Constant logger. */
	private static final Logger logger =
			LoggerFactory.getLogger(AcmIhmFieldGroupeServiceImpl.class);

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The acm ihm field repository. */
	@Autowired
	private AcmIhmFieldGroupeRepository acmIhmFieldGroupeRepository;

	/** The user client. */
	@Autowired
	private UserClient userClient;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmIhmFieldGroupeService#find(com.acm.utils.dtos.AcmIhmFieldGroupeDTO)
	 */
	@Override
	public List<AcmIhmFieldGroupeDTO> find(AcmIhmFieldGroupeDTO acmIhmFieldGroupeDTO) {

		Preconditions.checkNotNull(acmIhmFieldGroupeDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);

		if (ACMValidationUtils.isNullOrEmpty(acmIhmFieldGroupeDTO.getAcmIhmField())
				&& ACMValidationUtils.isNullOrEmpty(acmIhmFieldGroupeDTO.getGroup())
				&& ACMValidationUtils.isNullOrEmpty(acmIhmFieldGroupeDTO.getGroup().getId())
				&& ACMValidationUtils.isNullOrEmpty(acmIhmFieldGroupeDTO.getGroup().getCode())) {
			return new ArrayList<>();
		}
		// init QHabilitation
		QAcmIhmFieldGroupe qAcmIhmFieldGroupe = QAcmIhmFieldGroupe.acmIhmFieldGroupe;

		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find only enabled data
		predicate.and(qAcmIhmFieldGroupe.enabled.eq(Boolean.TRUE));
		// find by acmIhmField
		if (!ACMValidationUtils.isNullOrEmpty(acmIhmFieldGroupeDTO.getAcmIhmField())) {
			predicate.and(qAcmIhmFieldGroupe.acmIhmField.id
					.eq(acmIhmFieldGroupeDTO.getAcmIhmField().getId()));
		}
		// find by group id
		if (!ACMValidationUtils.isNullOrEmpty(acmIhmFieldGroupeDTO.getGroup())
				&& !ACMValidationUtils.isNullOrEmpty(acmIhmFieldGroupeDTO.getGroup().getId())) {
			predicate.and(qAcmIhmFieldGroupe.group.id.eq(acmIhmFieldGroupeDTO.getGroup().getId()));
		}
		// find by group code
		if (!ACMValidationUtils.isNullOrEmpty(acmIhmFieldGroupeDTO.getGroup())
				&& !ACMValidationUtils.isNullOrEmpty(acmIhmFieldGroupeDTO.getGroup().getCode())) {
			predicate.and(
					qAcmIhmFieldGroupe.group.code.eq(acmIhmFieldGroupeDTO.getGroup().getCode()));
		}
		// QueryDSL using springDATA
		Iterable<AcmIhmFieldGroupe> iterable = acmIhmFieldGroupeRepository.findAll(predicate);
		List<AcmIhmFieldGroupe> acmIhmFieldGroupes = new ArrayList<>();
		iterable.forEach(acmIhmFieldGroupes::add);
		logger.info("{} : acmIhmFieldGroupes was founded", acmIhmFieldGroupes.size());

		// mapping returned list
		List<AcmIhmFieldGroupeDTO> acmIhmFieldGroupeDTOs = new ArrayList<>();
		acmIhmFieldGroupes.forEach(acmIhmFieldGroupe -> acmIhmFieldGroupeDTOs
				.add(mapper.map(acmIhmFieldGroupe, AcmIhmFieldGroupeDTO.class)));
		logger.info("Returning founded data ...");
		return acmIhmFieldGroupeDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmIhmFieldGroupeService#updateHabilitation(com.acm.utils.dtos.
	 * AcmIhmFieldGroupeDTO)
	 */
	@Override
	public AcmIhmFieldGroupeDTO updateHabilitation(AcmIhmFieldGroupeDTO acmIhmFieldGroupeDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(acmIhmFieldGroupeDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.info("Update acmIhmFieldGroupe with field ID = {}",
				acmIhmFieldGroupeDTO.getAcmIhmField().getId());

		AcmIhmFieldGroupeDTO oldAcmIhmFieldGroupeDTO =
				find(acmIhmFieldGroupeDTO).stream().findFirst().orElse(null);
		// if AcmIhmFieldGroupe already exist then update HABILITATION
		if (!ACMValidationUtils.isNullOrEmpty(oldAcmIhmFieldGroupeDTO)) {
			AcmIhmFieldGroupe oldAcmIhmFieldGroupe =
					mapper.map(oldAcmIhmFieldGroupeDTO, AcmIhmFieldGroupe.class);
			oldAcmIhmFieldGroupe.setHabilitation(acmIhmFieldGroupeDTO.getHabilitation());

			oldAcmIhmFieldGroupe.setEnabled(Boolean.TRUE);
			CommonFunctions.mapperToUpdate(oldAcmIhmFieldGroupe, userClient, logger);
			return mapper.map(acmIhmFieldGroupeRepository.save(oldAcmIhmFieldGroupe),
					AcmIhmFieldGroupeDTO.class);
		}
		// else create new AcmIhmFieldGroupe
		else {
			oldAcmIhmFieldGroupeDTO = new AcmIhmFieldGroupeDTO();
			oldAcmIhmFieldGroupeDTO.setHabilitation(acmIhmFieldGroupeDTO.getHabilitation());
			oldAcmIhmFieldGroupeDTO.setAcmIhmField(acmIhmFieldGroupeDTO.getAcmIhmField());
			oldAcmIhmFieldGroupeDTO.setGroup(acmIhmFieldGroupeDTO.getGroup());
			return save(oldAcmIhmFieldGroupeDTO);
		}

	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmIhmFieldGroupeService#save(com.acm.utils.dtos.AcmIhmFieldGroupeDTO)
	 */
	@Override
	public AcmIhmFieldGroupeDTO save(AcmIhmFieldGroupeDTO acmIhmFieldGroupeDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(acmIhmFieldGroupeDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);

		AcmIhmFieldGroupe acmIhmFieldGroupe =
				mapper.map(acmIhmFieldGroupeDTO, AcmIhmFieldGroupe.class);
		CommonFunctions.mapperToSave(acmIhmFieldGroupe, userClient, logger);
		AcmIhmFieldGroupe newAcmIhmFieldGroupe =
				acmIhmFieldGroupeRepository.save(acmIhmFieldGroupe);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, AcmIhmFieldGroupe.class.getSimpleName());
		return mapper.map(newAcmIhmFieldGroupe, AcmIhmFieldGroupeDTO.class);
	}

}
