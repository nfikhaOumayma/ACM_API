/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.GroupeUsersFoundException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.GroupeDTO;
import com.acm.utils.dtos.pagination.GroupePaginationDTO;

/**
 * {@link GroupeService} interface.
 *
 * @author HaythemBenizid
 * @since 0.3.0
 */
public interface GroupeService {

	/**
	 * Find {@link List} of {@link GroupeDTO} by given params.
	 * 
	 * @author HaythemBenizid
	 * @param groupeDTO the groupe DTO
	 * @return the list
	 */
	List<GroupeDTO> find(GroupeDTO groupeDTO);

	/**
	 * Find {@link List} of {@link GroupeDTO}.
	 * 
	 * @author MoezMhiri
	 * @return the list
	 */
	List<GroupeDTO> find();

	/**
	 * update groupe data in DB.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @param groupeDTO the groupe DTO
	 * @return the groupe DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	GroupeDTO save(Long id, GroupeDTO groupeDTO) throws ResourcesNotFoundException;

	/**
	 * Save groupe data in DB.
	 * 
	 * @author HaythemBenizid
	 * @param groupeDTO the groupe DTO
	 * @return the groupe DTO
	 * @throws ResourcesNotFoundException the Resources Not Found Exception
	 */
	GroupeDTO save(GroupeDTO groupeDTO) throws ResourcesNotFoundException;

	/**
	 * update groupe enabled in DB (use in setting module).
	 *
	 * @author MoezMhiri
	 * @param groupeDTO the groupe DTO
	 * @return the groupe DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws GroupeUsersFoundException the users found exception
	 */
	GroupeDTO updateEnabled(GroupeDTO groupeDTO)
			throws ResourcesNotFoundException, GroupeUsersFoundException;

	/**
	 * Find {@link GroupePaginationDTO} by page size & page number & given params
	 * ({@link GroupePaginationDTO}).
	 * 
	 * @author MoezMhiri
	 * @param groupePaginationDTO the groupe pagination DTO
	 * @return the list
	 */
	GroupePaginationDTO find(GroupePaginationDTO groupePaginationDTO);

	/**
	 * FindByCode {@link GroupeDTO} by given ID.
	 *
	 * @author MoezMhiri
	 * @param code the code
	 * @return the GroupeService DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	GroupeDTO findByCode(String code) throws ResourcesNotFoundException;

}
