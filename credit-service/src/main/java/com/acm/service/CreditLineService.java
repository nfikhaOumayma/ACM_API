package com.acm.service;

import java.util.List;

import com.acm.utils.dtos.AcmCreditLineDTO;
import com.acm.utils.dtos.AcmCreditLinePaginationDTO;
import com.acm.utils.dtos.AcmToppedUpHistoryDTO;

/**
 * The Interface CreditLineService.
 */
public interface CreditLineService {

	/**
	 * Save.
	 *
	 * @param acmCreditLineDTO the acm credit line DTO
	 * @return the acm credit line DTO
	 */
	AcmCreditLineDTO save(AcmCreditLineDTO acmCreditLineDTO);

	/**
	 * Find.
	 *
	 * @param acmCreditLinePaginationDTO the acm credit line pagination DTO
	 * @return the acm credit line pagination DTO
	 */
	AcmCreditLinePaginationDTO find(AcmCreditLinePaginationDTO acmCreditLinePaginationDTO);

	/**
	 * Delete topped up histories.
	 *
	 * @param toppedUpHistories the topped up histories
	 */
	void deleteToppedUpHistories(List<AcmToppedUpHistoryDTO> toppedUpHistories);
}
