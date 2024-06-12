package com.acm.api_emdha.service;

import com.acm.utils.dtos.LoanDTO;

/**
 * The Interface EmdhaApiService.
 */
public interface EmdhaApiService {

	/**
	 * Emdha api.
	 *
	 * @param loanDTO the loan DTO
	 * @return true, if successful
	 */
	boolean emdhaApi(LoanDTO loanDTO);

}
