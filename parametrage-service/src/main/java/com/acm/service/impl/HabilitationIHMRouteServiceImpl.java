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

import com.acm.constants.common.CommonConstants;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.repository.HabilitationIHMRouteRepository;
import com.acm.service.HabilitationIHMRouteService;
import com.acm.utils.dtos.HabilitationIHMRouteDTO;
import com.acm.utils.models.HabilitationIHMRoute;
import com.acm.utils.models.QHabilitationIHMRoute;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * The {@link HabilitationIHMRouteServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 0.9.0
 */
@Service
public class HabilitationIHMRouteServiceImpl implements HabilitationIHMRouteService {

	/** Default Mode is INFO. */
	private static final Logger logger =
			LoggerFactory.getLogger(HabilitationIHMRouteServiceImpl.class);

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The habilitationIHMRoute repository. */
	@Autowired
	private HabilitationIHMRouteRepository habilitationIHMRouteRepository;

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.HabilitationIHMRouteService#find(com.acm.utils.dtos.HabilitationIHMRouteDTO)
	 */
	@Override
	public List<HabilitationIHMRouteDTO> find(HabilitationIHMRouteDTO habilitationIHMRouteDTO) {

		Preconditions.checkNotNull(habilitationIHMRouteDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// init QHabilitationIHMRoute
		QHabilitationIHMRoute qHabilitationIHMRoute = QHabilitationIHMRoute.habilitationIHMRoute;

		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find only enabled data
		predicate.and(qHabilitationIHMRoute.enabled.eq(Boolean.TRUE));

		// find by Client
		predicate.and(qHabilitationIHMRoute.client.eq(CommonConstants.APP_NAME));

		// find by ID
		if (!ACMValidationUtils.isNullOrEmpty(habilitationIHMRouteDTO.getId())) {
			predicate.and(qHabilitationIHMRoute.id.eq(habilitationIHMRouteDTO.getId()));
		}

		// find by Code IHM Route
		if (!ACMValidationUtils.isNullOrEmpty(habilitationIHMRouteDTO.getCodeIHMRoute())) {
			predicate.and(qHabilitationIHMRoute.codeIHMRoute
					.eq(habilitationIHMRouteDTO.getCodeIHMRoute()));
		}

		// find by Settings Workflow
		if (!ACMValidationUtils.isNullOrEmpty(habilitationIHMRouteDTO.getSettingsWorkflow())) {
			predicate.and(qHabilitationIHMRoute.settingsWorkflow
					.eq(habilitationIHMRouteDTO.getSettingsWorkflow()));
		}

		// QueryDSL using springDATA
		Iterable<HabilitationIHMRoute> iterable = habilitationIHMRouteRepository.findAll(predicate);
		List<HabilitationIHMRoute> habilitationIHMRoutes = new ArrayList<>();
		iterable.forEach(habilitationIHMRoutes::add);
		logger.info("{} : habilitationIHMRoutes was founded", habilitationIHMRoutes.size());

		// mapping returned list
		List<HabilitationIHMRouteDTO> habilitationIHMRouteDTOs = new ArrayList<>();
		habilitationIHMRoutes.forEach(habilitationIHMRoute -> habilitationIHMRouteDTOs
				.add(mapper.map(habilitationIHMRoute, HabilitationIHMRouteDTO.class)));
		return habilitationIHMRouteDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.HabilitationIHMRouteService#findAll()
	 */
	@Override
	public List<HabilitationIHMRouteDTO> findAll() {

		List<HabilitationIHMRoute> habilitationIHMRoutes = habilitationIHMRouteRepository.findAll();
		// mapping returned list
		List<HabilitationIHMRouteDTO> habilitationIHMRouteDTOs = new ArrayList<>();
		habilitationIHMRoutes.forEach(habilitationIHMRouteDTO -> habilitationIHMRouteDTOs
				.add(mapper.map(habilitationIHMRouteDTO, HabilitationIHMRouteDTO.class)));

		logger.info("Returning founded data ...");
		return habilitationIHMRouteDTOs;
	}
}
