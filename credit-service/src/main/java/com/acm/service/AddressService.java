/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.AddressDTO;

// TODO: Auto-generated Javadoc
/**
 * {@link AddressService} interface.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
public interface AddressService {

	/**
	 * Find {@link AddressDTO} by given ID.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @return the address DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	AddressDTO find(Long id) throws ResourcesNotFoundException;

	/**
	 * Find {@link List} of {@link AddressDTO} by given params.
	 *
	 * @author HaythemBenizid
	 * @param addressDTO the address DTO
	 * @return the list
	 */
	List<AddressDTO> find(AddressDTO addressDTO);

	/**
	 * The method used for saving the given {@link AddressDTO}.
	 *
	 * @author HaythemBenizid
	 * @param addressDTO the address DTO
	 * @return the address DTO
	 */
	AddressDTO save(AddressDTO addressDTO);

	/**
	 * The method used for saving the given {@link AddressDTO} (USING ONLY BY BATCH).
	 *
	 * @author HaythemBenizid
	 * @param addressDTO the address DTO
	 * @return the address DTO
	 */
	AddressDTO saveByBatch(AddressDTO addressDTO);

	/**
	 * The method used for updating the given {@link AddressDTO} by ID.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @param addressDTO the address DTO
	 * @return the address DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	AddressDTO save(Long id, AddressDTO addressDTO) throws ResourcesNotFoundException;

	/**
	 * Delete {@link customerId} by customer Id.
	 * 
	 * @author MoezMhiri
	 * @param customerId the customerId
	 */
	void deleteAll(Long customerId);

	/**
	 * Delete {@link address} by address DTO.
	 *
	 * @author YesserSomai
	 * @param addressDTO the address DTO
	 */
	void delete(AddressDTO addressDTO);

	/**
	 * Save all Address used in customer 360 screen.
	 *
	 * @author YesserSOmai
	 * @param addressDTO the address DTO
	 * @return the address DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	AddressDTO saveAll(AddressDTO addressDTO) throws ResourcesNotFoundException;
	
	/**
	 * Save all address.
	 *
	 * @param addressDTOlst the address DT olst
	 * @return the list
	 */
	List<AddressDTO> saveAllAddress(List<AddressDTO> addressDTOlst);
}
