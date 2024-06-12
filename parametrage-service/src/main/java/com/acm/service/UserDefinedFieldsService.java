/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.dtos.UserDefinedFieldsDTO;

/**
 * {@link UserDefinedFieldsService} interface.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
public interface UserDefinedFieldsService {

	/**
	 * Find {@link UserDefinedFieldsDTO} by given ID.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @return the userDefinedFields DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	UserDefinedFieldsDTO find(Long id) throws ResourcesNotFoundException;

	/**
	 * Find {@link List} of {@link UserDefinedFieldsDTO} by given params.
	 *
	 * @author HaythemBenizid
	 * @param userDefinedFieldsDTO the userDefinedFields DTO
	 * @return the list
	 */
	List<UserDefinedFieldsDTO> find(UserDefinedFieldsDTO userDefinedFieldsDTO);

	/**
	 * The method used for saving the given {@link UserDefinedFieldsDTO}.
	 *
	 * @author HaythemBenizid
	 * @param userDefinedFieldsDTO the userDefinedFields DTO
	 * @param userDTO the user DTO
	 * @return the userDefinedFields DTO
	 */
	UserDefinedFieldsDTO save(UserDefinedFieldsDTO userDefinedFieldsDTO, UserDTO userDTO);

	/**
	 * The method used for updating the given {@link UserDefinedFieldsDTO} by ID.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @param userDefinedFieldsDTO the userDefinedFields DTO
	 * @return the userDefinedFields DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	UserDefinedFieldsDTO save(Long id, UserDefinedFieldsDTO userDefinedFieldsDTO)
			throws ResourcesNotFoundException;

	/**
	 * Load setting address from abacus DB (AddressType, AddressList, AddressListValue,
	 * SettingsAddress).
	 *
	 * @author HaythemBenizid
	 * @param userDTO the user DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	void loadSettingFromAbacus(UserDTO userDTO) throws ResourcesNotFoundException;

	/**
	 * Find UDF field by list ids.
	 * 
	 * @author idridi
	 * @param userDefinedFieldsDTOs the user defined fields DT os
	 * @return the list
	 */
	List<UserDefinedFieldsDTO> findUDFFieldByListIds(
			List<UserDefinedFieldsDTO> userDefinedFieldsDTOs);

	/**
	 * Find UDF field by ids.
	 *
	 * @param udfFieldIds the udf field ids
	 * @return the list
	 */
	List<UserDefinedFieldsDTO> findUDFFieldByIds(List<Long> udfFieldIds);

}
