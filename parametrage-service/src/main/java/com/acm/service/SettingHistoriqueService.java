/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.SettingHistoriqueDTO;

/**
 * {@link SettingHistoriqueService} interface.
 *
 * @author HaythemBenizid
 * @since 1.0.2
 */
public interface SettingHistoriqueService {

	/**
	 * Find {@link SettingHistoriqueDTO} by given id.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @return the setting historique DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	SettingHistoriqueDTO find(Long id) throws ResourcesNotFoundException;

	/**
	 * Find by given params.
	 *
	 * @author HaythemBenizid
	 * @param settingHistoriqueDTO the setting historique DTO
	 * @return the list
	 */
	List<SettingHistoriqueDTO> find(SettingHistoriqueDTO settingHistoriqueDTO);

	/**
	 * The method used for saving the given {@link SettingHistoriqueDTO}.
	 * 
	 * @author HaythemBenizid
	 * @param settingHistoriqueDTO the settingHistorique DTO
	 * @return the settingHistorique DTO
	 */
	SettingHistoriqueDTO save(SettingHistoriqueDTO settingHistoriqueDTO);

}
