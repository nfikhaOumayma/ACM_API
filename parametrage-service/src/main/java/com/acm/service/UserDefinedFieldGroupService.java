/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.dtos.UserDefinedFieldGroupDTO;

/**
 * {@link UserDefinedFieldGroupService} interface.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
public interface UserDefinedFieldGroupService {

	/**
	 * Find {@link UserDefinedFieldGroupDTO} by given ID.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @return the userDefinedFieldGroup DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	UserDefinedFieldGroupDTO find(Long id) throws ResourcesNotFoundException;

	/**
	 * Find {@link List} of {@link UserDefinedFieldGroupDTO} by given params.
	 *
	 * @author HaythemBenizid
	 * @param userDefinedFieldGroupDTO the userDefinedFieldGroup DTO
	 * @return the list
	 */
	List<UserDefinedFieldGroupDTO> find(UserDefinedFieldGroupDTO userDefinedFieldGroupDTO);

	/**
	 * The method used for saving the given {@link UserDefinedFieldGroupDTO}.
	 * 
	 * @author HaythemBenizid
	 * @param userDefinedFieldGroupDTO the userDefinedFieldGroup DTO
	 * @param userDTO the user DTO
	 * @return the userDefinedFieldGroup DTO
	 */
	UserDefinedFieldGroupDTO save(UserDefinedFieldGroupDTO userDefinedFieldGroupDTO,
			UserDTO userDTO);

	/**
	 * The method used for updating the given {@link UserDefinedFieldGroupDTO} by ID.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @param userDefinedFieldGroupDTO the userDefinedFieldGroup DTO
	 * @return the userDefinedFieldGroup DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	UserDefinedFieldGroupDTO save(Long id, UserDefinedFieldGroupDTO userDefinedFieldGroupDTO)
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
	 * Find by ids.
	 *
	 * @param ids the ids
	 * @return the list
	 */
	List<UserDefinedFieldGroupDTO> findByIds(List<Long> ids);

}
