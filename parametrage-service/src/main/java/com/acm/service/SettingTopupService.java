/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ParametrageException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.SettingTopupDTO;
import com.acm.utils.dtos.SettingTopupValidityDTO;

/**
 * {@link SettingTopupService} class.
 *
 * @author mlamloum
 * @since 0.1.0
 */
public interface SettingTopupService {

	/**
	 * Find.
	 * 
	 * @author mlamloum
	 * @param settingTopupDTO the setting topup DTO
	 * @return the list
	 */
	List<SettingTopupDTO> find(SettingTopupDTO settingTopupDTO);

	/**
	 * Save.
	 *
	 * @author mlamloum
	 * @param settingTopupDTO the setting topup DTO
	 * @return the setting topup DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	SettingTopupDTO save(SettingTopupDTO settingTopupDTO) throws ResourcesNotFoundException;

	/**
	 * Save.
	 * 
	 * @author mlamloum
	 * @param id the id
	 * @param settingTopupDTO the setting topup DTO
	 * @return the setting topup DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	SettingTopupDTO save(Long id, SettingTopupDTO settingTopupDTO)
			throws ResourcesNotFoundException;

	/**
	 * Check validity.
	 *
	 * @author mlamloum
	 * @param loanDTO the loan DTO
	 * @return the setting topup validity DTO
	 * @throws ParametrageException the parametrage exception
	 */
	SettingTopupValidityDTO checkValidity(LoanDTO loanDTO) throws ParametrageException;
}
