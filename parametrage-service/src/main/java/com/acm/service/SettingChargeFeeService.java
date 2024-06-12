package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.SettingChargeFeeDTO;

/**
 * The Interface SettingChargeFeeService.
 */
public interface SettingChargeFeeService {

	/**
	 * Find.
	 *
	 * @param SettingChargeFeeDTO the setting charge fee DTO
	 * @return the list
	 */
	List<SettingChargeFeeDTO> find(SettingChargeFeeDTO SettingChargeFeeDTO);

	/**
	 * Find.
	 *
	 * @param id the id
	 * @return the setting charge fee DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	SettingChargeFeeDTO find(Long id) throws ResourcesNotFoundException;

	/**
	 * Find All.
	 *
	 * @return the list
	 */
	List<SettingChargeFeeDTO> find();

	/**
	 * Save.
	 *
	 * @param id the id
	 * @param settingChargeFeeDTO the setting charge fee DTO
	 * @return the setting charge fee DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	SettingChargeFeeDTO save(Long id, SettingChargeFeeDTO settingChargeFeeDTO)
			throws ResourcesNotFoundException;

	/**
	 * Save.
	 *
	 * @param settingChargeFeeDTO the setting charge fee DTO
	 * @return the setting charge fee DTO
	 */
	SettingChargeFeeDTO save(SettingChargeFeeDTO settingChargeFeeDTO);
}
