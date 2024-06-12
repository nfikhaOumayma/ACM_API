/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.utils.dtos.AddressHistoriqueDTO;

/**
 * {@link AddressHistoriqueService} interface.
 *
 * @author YesserSomai
 * @since 1.0.14
 */
public interface AddressHistoriqueService {

	/**
	 * Find {@link List} of {@link AddressHistoriqueDTO} by given params.
	 *
	 * @author YesserSomai
	 * @param addressHistoriqueDTO the addressHistorique DTO
	 * @return the list
	 */
	List<AddressHistoriqueDTO> find(AddressHistoriqueDTO addressHistoriqueDTO);

	/**
	 * The method used for saving the given {@link AddressHistoriqueDTO}.
	 *
	 * @author YesserSomai
	 * @param addressDTO the address DTO
	 * @return the address DTO
	 */
	AddressHistoriqueDTO save(AddressHistoriqueDTO addressDTO);
}
