/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.utils.dtos.HabilitationIHMRouteDTO;

/**
 * The {@link HabilitationIHMRouteService} class.
 *
 * @author HaythemBenizid
 * @since 0.9.0
 */
public interface HabilitationIHMRouteService {

	/**
	 * search for habilitation by given params.
	 *
	 * @author HaythemBenizid
	 * @param habilitationIHMRouteDTO the habilitation IHM route DTO
	 * @return the list
	 */
	List<HabilitationIHMRouteDTO> find(HabilitationIHMRouteDTO habilitationIHMRouteDTO);

	/**
	 * search for habilitation .
	 *
	 * @author MoezMhiri
	 * @return the list
	 */
	List<HabilitationIHMRouteDTO> findAll();
}
