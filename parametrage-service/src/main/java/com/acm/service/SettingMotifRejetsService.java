/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.CodeSettingExistException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.SettingMotifRejetsDTO;

/**
 * {@link SettingMotifRejetsService} interface.
 *
 * @author HaythemBenizid
 * @since 0.2.0
 */
public interface SettingMotifRejetsService {

	/**
	 * Find {@link List} of {@link SettingMotifRejetsDTO} by given params. using Querydsl.
	 *
	 * @author HaythemBenizid
	 * @param settingMotifRejetsDTO the setting document DTO
	 * @return the list
	 */
	List<SettingMotifRejetsDTO> find(SettingMotifRejetsDTO settingMotifRejetsDTO);

	/**
	 * The method used for saving the given {@link SettingMotifRejetsDTO}.
	 *
	 * @author HaythemBenizid
	 * @param settingMotifRejetsDTO the settingMotifRejets DTO
	 * @return the SettingMotifRejetsService DTO
	 * @throws CodeSettingExistException the code setting exist exception
	 */
	SettingMotifRejetsDTO save(SettingMotifRejetsDTO settingMotifRejetsDTO)
			throws CodeSettingExistException;

	/**
	 * The method used for updating the given {@link SettingMotifRejetsDTO} by ID.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @param settingMotifRejetsDTO the settingMotifRejets DTO
	 * @return the SettingMotifRejetsService DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CodeSettingExistException the code setting exist exception
	 */
	SettingMotifRejetsDTO save(Long id, SettingMotifRejetsDTO settingMotifRejetsDTO)
			throws ResourcesNotFoundException, CodeSettingExistException;

	/**
	 * Find {@link List} of {@link SettingMotifRejetsDTO}.
	 *
	 * @author YesserSomai
	 * @return the list
	 */
	List<SettingMotifRejetsDTO> find();

	/**
	 * Delete.
	 * 
	 * @author idridi
	 * @param id the id
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	void delete(Long id) throws ResourcesNotFoundException;

}
