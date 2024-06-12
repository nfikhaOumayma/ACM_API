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

import com.acm.service.LoanHistoriqueService;
import com.acm.utils.dtos.CustomerDecisionDTO;
import com.acm.utils.dtos.LoanApprovalHistoriqueDTO;
import com.acm.utils.dtos.LoanHistoriqueDTO;
import com.acm.utils.dtos.LoanNoteHistoriqueDTO;

/**
 * This class @{link LoanHistoriqueController} used to control all the LoanHistorique requests.
 *
 * @author HaythemBenizid
 * @since 0.6.0
 */
@RestController
@RequestMapping("/loan-historiques")
public class LoanHistoriqueController {

	/** The LoanHistorique service. */
	@Autowired
	private LoanHistoriqueService loanHistoriqueService;

	/**
	 * Find list LoanHistorique by given params.
	 * 
	 * @author HaythemBenizid
	 * @param loanHistoriqueDTO the loan historique DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<LoanHistoriqueDTO> find(@RequestBody LoanHistoriqueDTO loanHistoriqueDTO) {

		return loanHistoriqueService.find(loanHistoriqueDTO);
	}

	/**
	 * Find List of {@link LoanNoteHistoriqueDTO} => Combine {@link LoanApprovalHistoriqueDTO} &&
	 * {@link CustomerDecisionDTO} by given params order bydateUpdate DESC.
	 *
	 * @author HaythemBenizid
	 * @param loanNoteHistoriqueDTO the loan note historique DTO
	 * @return the list
	 */
	@PostMapping("/notes")
	public List<LoanNoteHistoriqueDTO> find(
			@RequestBody LoanNoteHistoriqueDTO loanNoteHistoriqueDTO) {

		return loanHistoriqueService.find(loanNoteHistoriqueDTO);
	}

}
