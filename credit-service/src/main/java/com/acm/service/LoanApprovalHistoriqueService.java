/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.LoanApprovalHistoriqueDTO;

/**
 * {@link LoanApprovalHistoriqueService} interface.
 *
 * @author HaythemBenizid
 * @since 0.6.0
 */
public interface LoanApprovalHistoriqueService {

	/**
	 * Find {@link LoanApprovalHistoriqueDTO} by given ID.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @return the loanApprovalHistorique DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	LoanApprovalHistoriqueDTO find(Long id) throws ResourcesNotFoundException;

	/**
	 * Find {@link List} of {@link LoanApprovalHistoriqueDTO} by given params.
	 * 
	 * @author HaythemBenizid
	 * @param loanApprovalHistoriqueDTO the loanApprovalHistorique DTO
	 * @return the list
	 */
	List<LoanApprovalHistoriqueDTO> find(LoanApprovalHistoriqueDTO loanApprovalHistoriqueDTO);

	/**
	 * The method used for saving the given {@link LoanApprovalHistoriqueDTO}.
	 * 
	 * @author HaythemBenizid
	 * @param loanApprovalHistoriqueDTO the loanApprovalHistorique DTO
	 * @return the loanApprovalHistorique DTO
	 */
	LoanApprovalHistoriqueDTO saveAndSetApprovalLabel(LoanApprovalHistoriqueDTO loanApprovalHistoriqueDTO);
	
	/**
	 * Save.
	 * @author mlamloum
	 * @param loanApprovalHistoriqueDTO the loan approval historique DTO
	 * @return the loan approval historique DTO
	 */
	LoanApprovalHistoriqueDTO save(LoanApprovalHistoriqueDTO loanApprovalHistoriqueDTO);

}
