/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.AddressSettingDTO;

/**
 * {@link AddressSettingService} interface.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
public interface AddressSettingService {

	/**
	 * Find {@link AddressSettingDTO} by given ID.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @return the addressSetting DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	AddressSettingDTO find(Long id) throws ResourcesNotFoundException;

	/**
	 * Find {@link List} of {@link AddressSettingDTO} by given params.
	 *
	 * @author HaythemBenizid
	 * @param addressSettingDTO the addressSetting DTO
	 * @return the list
	 */
	List<AddressSettingDTO> find(AddressSettingDTO addressSettingDTO);

	/**
	 * The method used for saving the given {@link AddressSettingDTO}.
	 *
	 * @author HaythemBenizid
	 * @param addressSettingDTO the addressSetting DTO
	 * @return the addressSetting DTO
	 */
	AddressSettingDTO save(AddressSettingDTO addressSettingDTO);

	/**
	 * The method used for updating the given {@link AddressSettingDTO} by ID.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @param addressSettingDTO the addressSetting DTO
	 * @return the addressSetting DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	AddressSettingDTO save(Long id, AddressSettingDTO addressSettingDTO)
			throws ResourcesNotFoundException;

	/**
	 * Load setting address from abacus DB (AddressType, AddressList, AddressListValue,
	 * SettingsAddress).
	 * 
	 * @author HaythemBenizid
	 */
	void loadSettingFromAbacus();

	/**
	 * Reset setting address from ABACUS DB.
	 * 
	 * @author HaythemBenizid
	 */
	void resetSettingFromAbacus();

	/**
	 * Save all.
	 *
	 * @author kouali
	 * @param addressSettingDTOs the address setting DT os
	 * @return the address setting DTO
	 */
	AddressSettingDTO saveAll(List<AddressSettingDTO> addressSettingDTOs);
}
