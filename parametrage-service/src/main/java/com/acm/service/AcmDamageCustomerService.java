/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.AcmDamagedCustomerDTO;

/**
 * The Interface AcmDamageCustomerService.
 */
public interface AcmDamageCustomerService {

	/**
	 * Find.
	 * 
	 * @author idridi
	 * @param id the id
	 * @return the acm damaged customer DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	AcmDamagedCustomerDTO find(Long id) throws ResourcesNotFoundException;

	/**
	 * Save.
	 * 
	 * @author idridi
	 * @param acmDamagedCustomerDTO the acm damaged customer DTO
	 * @return the acm damaged customer DTO
	 */
	AcmDamagedCustomerDTO save(AcmDamagedCustomerDTO acmDamagedCustomerDTO);

}