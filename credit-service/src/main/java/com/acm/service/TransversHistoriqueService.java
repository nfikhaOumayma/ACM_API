/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.TransversHistoriqueDTO;

/**
 * {@link TransversHistoriqueService} interface.
 *
 * @author MoezMhiri
 * @since 1.0.12
 */
public interface TransversHistoriqueService {

	/**
	 * Find {@link TransversHistoriqueDTO} by given ID.
	 * 
	 * @author MoezMhiri
	 * @param id the id
	 * @return the transvers historique DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	TransversHistoriqueDTO find(Long id) throws ResourcesNotFoundException;

	/**
	 * Find {@link List} of {@link TransversHistoriqueDTO} by given params.
	 * 
	 * @author MoezMhiri
	 * @param transversHistoriqueDTO the transvers historique DTO
	 * @return the list
	 */
	List<TransversHistoriqueDTO> find(TransversHistoriqueDTO transversHistoriqueDTO);

	/**
	 * The method used for saving the given {@link TransversHistoriqueDTO}.
	 * 
	 * @author MoezMhiri
	 * @param transversHistoriqueDTO the transvers historique DTO
	 * @return the transvers historique DTO
	 */
	TransversHistoriqueDTO save(TransversHistoriqueDTO transversHistoriqueDTO);

	/**
	 * The method used for updating the given {@link TransversHistoriqueDTO} data.
	 * 
	 * @author MoezMhiri
	 * @param id the id
	 * @param transversHistoriqueDTO the transvers historique DTO
	 * @return the transvers historique DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	TransversHistoriqueDTO save(Long id, TransversHistoriqueDTO transversHistoriqueDTO)
			throws ResourcesNotFoundException;

}
