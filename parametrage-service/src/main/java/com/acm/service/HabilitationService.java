/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.text.ParseException;
import java.util.List;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.HabilitationDTO;

/**
 * The {@link HabilitationService} class.
 *
 * @author HaythemBenizid
 * @since 0.3.0
 */
public interface HabilitationService {

	/**
	 * Find all habilitations in db relative to connected user.
	 *
	 * @author HaythemBenizid
	 * @return an array {@link List} of the specified object {@link HabilitationDTO}
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws Exception the exception
	 */
	List<HabilitationDTO> find() throws ResourcesNotFoundException, Exception;

	/**
	 * search for habilitation by given params.
	 * 
	 * @author HaythemBenizid
	 * @param habilitationDTO the search habilitation
	 * @return the list
	 */
	List<HabilitationDTO> find(HabilitationDTO habilitationDTO);

	/**
	 * Save habilitation data in DB.
	 * 
	 * @author MoezMhiri
	 * @param habilitationDTO the habiliation DTO
	 * @return the habiliation DTO
	 */
	HabilitationDTO save(HabilitationDTO habilitationDTO);

	/**
	 * search for habilitation.
	 * 
	 * @author MoezMhiri
	 * @return the list
	 * @throws ResourcesNotFoundException the Resources Not Found Exception
	 */
	List<HabilitationDTO> findByGroupeID() throws ResourcesNotFoundException;

	/**
	 * save all habilitaion.
	 * 
	 * @author MoezMhiri
	 * @param habilitationDTOs the list habiliation DTO
	 */
	void saveAll(List<HabilitationDTO> habilitationDTOs);

	/**
	 * Find all habilitations.
	 * 
	 * @author MoezMhiri
	 * @param habilitationDTO the habiliation DTO
	 * @return the habiliation DTO
	 */
	List<HabilitationDTO> findAll(HabilitationDTO habilitationDTO);

	/**
	 * update all habilitations.
	 * 
	 * @author MoezMhiri
	 * @param habilitationDTOs the list habiliation DTO
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	List<HabilitationDTO> updateAll(List<HabilitationDTO> habilitationDTOs)
			throws ResourcesNotFoundException;

	/**
	 * Gets the mac adress from server.
	 *
	 * @return the mac adress from server
	 * @throws ParseException the parse exception
	 */
	String getMacAdressFromServer() throws ParseException;

}
