/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.utils.dtos.GuarantorDTO;

/**
 * {@link GuarantorAbacusService} interface.
 *
 * @author HaythemBenizid
 * @since 0.2.0
 */
public interface GuarantorAbacusService {

	/**
	 * Find {@link List} of {@link GuarantorDTO} data by loan ID.
	 *
	 * @author HaythemBenizid
	 * @param idLoan the id loan
	 * @return the list guarantor DTO
	 */
	List<GuarantorDTO> find(Long idLoan);
}
