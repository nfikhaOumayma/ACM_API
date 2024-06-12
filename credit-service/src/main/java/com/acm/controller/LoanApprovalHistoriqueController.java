/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.service.LoanApprovalHistoriqueService;
import com.acm.utils.dtos.LoanApprovalHistoriqueDTO;

/**
 * This class @{link LoanApprovalHistoriqueController} used to control all the
 * LoanApprovalHistorique requests.
 *
 * @author HaythemBenizid
 * @since 0.6.0
 */
@RestController
@RequestMapping("/loan-approval-historiques")
public class LoanApprovalHistoriqueController {

	/** The LoanApprovalHistorique service. */
	@Autowired
	private LoanApprovalHistoriqueService loanApprovalHistoriqueService;

	/**
	 * Find list LoanApprovalHistorique by given params.
	 * 
	 * @author HaythemBenizid
	 * @param loanApprovalHistoriqueDTO the loan approval historique DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<LoanApprovalHistoriqueDTO> find(
			@RequestBody LoanApprovalHistoriqueDTO loanApprovalHistoriqueDTO) {

		return loanApprovalHistoriqueService.find(loanApprovalHistoriqueDTO);
	}

	/**
	 * Creates the LoanApprovalHistoriqueDTO by new value.
	 * 
	 * @author HaythemBenizid
	 * @param loanApprovalHistoriqueDTO the loan approval historique DTO
	 * @return the loan approval historique DTO
	 */
	@PostMapping("/create")
	public LoanApprovalHistoriqueDTO create(
			@RequestBody LoanApprovalHistoriqueDTO loanApprovalHistoriqueDTO) {

		return loanApprovalHistoriqueService.saveAndSetApprovalLabel(loanApprovalHistoriqueDTO);
	}
}
