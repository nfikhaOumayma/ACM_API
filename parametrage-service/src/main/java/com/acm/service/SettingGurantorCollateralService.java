/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.SettingGurantorCollateralDTO;

/**
 * {@link SettingGurantorCollateralService2} class.
 *
 * @author HaythemBenizid
 * @since 0.8.0
 */
public interface SettingGurantorCollateralService {

	/**
	 * Find by given params.
	 *
	 * @author HaythemBenizid
	 * @param settingGurantorCollateralDTO the setting level DTO
	 * @return the list
	 */
	List<SettingGurantorCollateralDTO> find(
			SettingGurantorCollateralDTO settingGurantorCollateralDTO);

	/**
	 * The method used for saving the given {@link SettingGurantorCollateralDTO}.
	 *
	 * @author HaythemBenizid
	 * @param settingGurantorCollateralDTO the settingGurantorCollateral DTO
	 * @return the SettingGurantorCollateralService DTO
	 */
	SettingGurantorCollateralDTO save(SettingGurantorCollateralDTO settingGurantorCollateralDTO);

	/**
	 * The method used for updating the given {@link SettingGurantorCollateralDTO} by ID.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @param settingGurantorCollateralDTO the settingGurantorCollateral DTO
	 * @return the SettingGurantorCollateralService DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	SettingGurantorCollateralDTO save(Long id,
			SettingGurantorCollateralDTO settingGurantorCollateralDTO)
			throws ResourcesNotFoundException;

}
