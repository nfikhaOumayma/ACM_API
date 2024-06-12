package com.acm.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.service.CreditLineService;
import com.acm.utils.dtos.AcmCreditLineDTO;
import com.acm.utils.dtos.AcmCreditLinePaginationDTO;
import com.acm.utils.dtos.AcmToppedUpHistoryDTO;

/**
 * The Class CreditLineController.
 */
@RestController
@RequestMapping("/credit-line")
public class CreditLineController {

	/** The credit line service. */
	@Autowired
	CreditLineService creditLineService;

	/**
	 * Creates the.
	 *
	 * @param acmCreditLineDTO the acm credit line DTO
	 * @return the acm credit line DTO
	 */
	@PostMapping("/create")
	public AcmCreditLineDTO create(@RequestBody AcmCreditLineDTO acmCreditLineDTO) {

		return creditLineService.save(acmCreditLineDTO);
	}

	/**
	 * Find credit line pagination.
	 *
	 * @param acmCreditLinePaginationDTO the acm credit line pagination DTO
	 * @return the acm credit line pagination DTO
	 */
	@PostMapping("/find-pagination")
	public AcmCreditLinePaginationDTO findCreditLinePagination(
			@RequestBody AcmCreditLinePaginationDTO acmCreditLinePaginationDTO) {

		return creditLineService.find(acmCreditLinePaginationDTO);
	}

	/**
	 * Delete topped up histories.
	 *
	 * @param toppedUpHistories the topped up histories
	 */
	@PostMapping("/delete-toppedup-history")
	public void deleteToppedUpHistories(
			@RequestBody List<AcmToppedUpHistoryDTO> toppedUpHistories) {

		creditLineService.deleteToppedUpHistories(toppedUpHistories);
	}
}
