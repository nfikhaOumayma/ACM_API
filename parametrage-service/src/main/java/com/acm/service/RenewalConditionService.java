/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.RenewalConditionSettingLoanException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.CustomerAccountDTO;
import com.acm.utils.dtos.RenewalConditionDTO;
import com.acm.utils.dtos.RenewalConditionLoanDTO;

/**
 * {@link RenewalConditionService} interface.
 *
 * @author idridi
 * @since 1.0.8
 */
public interface RenewalConditionService {

	/**
	 * Find.
	 * 
	 * @author idridi
	 * @param idIncentiveSetting the id incentive setting
	 * @return the renewal condition DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	RenewalConditionDTO find(Long idIncentiveSetting) throws ResourcesNotFoundException;

	/**
	 * Find.
	 * 
	 * @author idridi
	 * @param renewalConditionDTO the renewal condition DTO
	 * @return the list
	 */
	List<RenewalConditionDTO> find(RenewalConditionDTO renewalConditionDTO);

	/**
	 * Save.
	 * 
	 * @author idridi
	 * @param renewalConditionDTO the renewal condition DTO
	 * @return the renewal condition DTO
	 */
	RenewalConditionDTO save(RenewalConditionDTO renewalConditionDTO);

	/**
	 * Save.
	 * 
	 * @author idridi
	 * @param id the id
	 * @param renewalConditionDTO the renewal condition DTO
	 * @return the renewal condition DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	RenewalConditionDTO save(Long id, RenewalConditionDTO renewalConditionDTO)
			throws ResourcesNotFoundException;

	/**
	 * Delete.
	 * 
	 * @author idridi
	 * @param id the id
	 */
	void delete(Long id);

	/**
	 * Find by year and last paid amount.
	 *
	 * @author idridi
	 * @param renewalYear the renewal year
	 * @param lastPaidAmount the last paid amount
	 * @return the renewal condition DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	RenewalConditionDTO findByYearAndLastPaidAmount(Integer renewalYear, Long lastPaidAmount)
			throws ResourcesNotFoundException;

	/**
	 * Find renewal condition setting.
	 *
	 * @author idridi
	 * @param customerAccountDTO the customer account DTO
	 * @return the renewal condition loan DTO
	 * @throws RenewalConditionSettingLoanException the renewal condition setting loan exception
	 */
	RenewalConditionLoanDTO findRenewalConditionSetting(CustomerAccountDTO customerAccountDTO)
			throws RenewalConditionSettingLoanException;

}
