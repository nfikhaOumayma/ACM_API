/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.ApplicationFeeDTO;
import com.acm.utils.dtos.AssetTypeListDTO;
import com.acm.utils.dtos.DeferredPeriodTypeDTO;
import com.acm.utils.dtos.PortfolioDTO;
import com.acm.utils.dtos.SettingListValuesDTO;

/**
 * {@link SettingListValuesService} interface.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
public interface SettingListValuesService {

	/**
	 * Find {@link SettingListValuesDTO} by given ID.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @return the settingListValues DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	SettingListValuesDTO find(Long id) throws ResourcesNotFoundException;

	/**
	 * Find {@link List} of {@link SettingListValuesDTO} by given params.
	 *
	 * @author HaythemBenizid
	 * @param settingListValuesDTO the settingListValues DTO
	 * @return the list
	 */
	List<SettingListValuesDTO> find(SettingListValuesDTO settingListValuesDTO);

	/**
	 * The method used for saving the given {@link SettingListValuesDTO}.
	 *
	 * @author HaythemBenizid
	 * @param settingListValuesDTO the settingListValues DTO
	 * @return the settingListValues DTO
	 */
	SettingListValuesDTO save(SettingListValuesDTO settingListValuesDTO);

	/**
	 * The method used for updating the given {@link SettingListValuesDTO} by ID.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @param settingListValuesDTO the settingListValues DTO
	 * @return the settingListValues DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	SettingListValuesDTO save(Long id, SettingListValuesDTO settingListValuesDTO)
			throws ResourcesNotFoundException;

	/**
	 * Load setting address from abacus DB .
	 *
	 * @author HaythemBenizid
	 */
	void loadSettingFromAbacus();

	/**
	 * Find list of ABACUS portfolios.
	 *
	 * @author Salmen Fatnassi
	 * @return the list of portfolios
	 */
	List<PortfolioDTO> findAllPortfolio();

	/**
	 * Reset setting address from ABACUS DB : load-list-value-setting.
	 * 
	 * @author HaythemBenizid
	 */
	void resetSettingFromAbacus();

	/**
	 * Find deferred period types.
	 *
	 * @author mlamloum
	 * @return the list
	 */
	List<DeferredPeriodTypeDTO> findDeferredPeriodTypes();

	/**
	 * Find fees.
	 *
	 * @return the list
	 */
	List<ApplicationFeeDTO> findFees();

	/**
	 * Convert json to array.
	 *
	 * @param jsonValue the json value
	 * @return the list
	 */
	List<AssetTypeListDTO> convertJsonToArray(String jsonValue);
}
