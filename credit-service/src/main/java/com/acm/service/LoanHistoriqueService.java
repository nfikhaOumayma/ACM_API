/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.CustomerDecisionDTO;
import com.acm.utils.dtos.LoanApprovalHistoriqueDTO;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.LoanHistoriqueDTO;
import com.acm.utils.dtos.LoanNoteHistoriqueDTO;

/**
 * {@link LoanHistoriqueService} interface.
 *
 * @author HaythemBenizid
 * @since 0.3.0
 */
public interface LoanHistoriqueService {

	/**
	 * Find {@link LoanHistoriqueDTO} by given ID.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @return the loanHistorique DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	LoanHistoriqueDTO find(Long id) throws ResourcesNotFoundException;

	/**
	 * Find {@link List} of {@link LoanHistoriqueDTO} by ID loan and default order by dateUpdate
	 * DESC.
	 * 
	 * @author HaythemBenizid
	 * @param loanHistoriqueDTO the loanHistorique DTO
	 * @return the list
	 */
	List<LoanHistoriqueDTO> find(LoanHistoriqueDTO loanHistoriqueDTO);

	/**
	 * The method used for saving the given {@link LoanHistoriqueDTO}.
	 * 
	 * @author HaythemBenizid
	 * @param loanHistoriqueDTO the loanHistorique DTO
	 * @return the loanHistorique DTO
	 */
	LoanHistoriqueDTO save(LoanHistoriqueDTO loanHistoriqueDTO);

	/**
	 * Find {@link List} of {@link LoanNoteHistoriqueDTO} => Combine
	 * {@link LoanApprovalHistoriqueDTO} && {@link CustomerDecisionDTO} by given params order by
	 * dateUpdate DESC.
	 * 
	 * @author HaythemBenizid
	 * @param loanNoteHistoriqueDTO the loan note historique DTO
	 * @return the list
	 */
	List<LoanNoteHistoriqueDTO> find(LoanNoteHistoriqueDTO loanNoteHistoriqueDTO);

	/**
	 * The method used for updating the category (ALERT) the given {@link LoanDTO}.
	 * 
	 * @author MoezMhiri
	 * @param loanDTO the loanDTO DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	LoanHistoriqueDTO saveForTimer(LoanDTO loanDTO) throws ResourcesNotFoundException;

	/**
	 * Save.
	 *
	 * @param loanHistoriqueDTO the loan historique DTO
	 * @param insertBy the insert by
	 * @return the loan historique DTO
	 */
	LoanHistoriqueDTO save(LoanHistoriqueDTO loanHistoriqueDTO, String insertBy);

}
