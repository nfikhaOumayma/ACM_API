/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.dtos.UserDefinedFieldListValuesDTO;

/**
 * {@link UserDefinedFieldListValuesService} interface.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
public interface UserDefinedFieldListValuesService {

	/**
	 * Find {@link UserDefinedFieldListValuesDTO} by given ID.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @return the userDefinedFieldListValues DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	UserDefinedFieldListValuesDTO find(Long id) throws ResourcesNotFoundException;

	/**
	 * Find by id UDF list value and enabled row only.
	 *
	 * @author yesser somai
	 * @param idUDFListValue the id UDF list value
	 * @return the list
	 */
	List<UserDefinedFieldListValuesDTO> findByIdUDFListValue(Long idUDFListValue);

	/**
	 * Find {@link List} of {@link UserDefinedFieldListValuesDTO} by given params.
	 *
	 * @author HaythemBenizid
	 * @param userDefinedFieldListValuesDTO the userDefinedFieldListValues DTO
	 * @return the list
	 */
	List<UserDefinedFieldListValuesDTO> find(
			UserDefinedFieldListValuesDTO userDefinedFieldListValuesDTO);

	/**
	 * The method used for saving the given {@link UserDefinedFieldListValuesDTO}.
	 *
	 * @author HaythemBenizid
	 * @param userDefinedFieldListValuesDTO the userDefinedFieldListValues DTO
	 * @param userDTO the user DTO
	 * @return the userDefinedFieldListValues DTO
	 */
	UserDefinedFieldListValuesDTO save(UserDefinedFieldListValuesDTO userDefinedFieldListValuesDTO,
			UserDTO userDTO);

	/**
	 * The method used for updating the given {@link UserDefinedFieldListValuesDTO} by ID.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @param userDefinedFieldListValuesDTO the userDefinedFieldListValues DTO
	 * @return the userDefinedFieldListValues DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	UserDefinedFieldListValuesDTO save(Long id,
			UserDefinedFieldListValuesDTO userDefinedFieldListValuesDTO)
			throws ResourcesNotFoundException;

	/**
	 * Load setting address from abacus DB (AddressType, AddressList, AddressListValue,
	 * SettingsAddress).
	 *
	 * @author HaythemBenizid
	 * @param userDTO the user DTO
	 */
	void loadSettingFromAbacus(UserDTO userDTO);

	/**
	 * Reset setting address from ABACUS DB : load-UDF-list-value AND load-fields-setting AND
	 * load-group-setting.
	 *
	 * @author HaythemBenizid
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	void resetSettingFromAbacus() throws ResourcesNotFoundException;

	/**
	 * Save all.
	 *
	 * @author kouali
	 * @param userDefinedFieldListValuesDTO the user defined field list values DTO
	 * @param userDTO the user DTO
	 * @return the list
	 */
	List<UserDefinedFieldListValuesDTO> saveAll(
			List<UserDefinedFieldListValuesDTO> userDefinedFieldListValuesDTO, UserDTO userDTO);	
}
