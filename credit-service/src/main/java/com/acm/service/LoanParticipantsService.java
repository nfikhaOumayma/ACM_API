/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.LoanParticipantsDTO;
import com.acm.utils.models.LoanParticipants;

/**
 * {@link LoanParticipantsService} interface.
 *
 * @author HaythemBenizid
 * @since 0.10.0
 */
public interface LoanParticipantsService {

	/**
	 * Find {@link List} of {@link LoanParticipantsDTO} by given params.
	 * 
	 * @author HaythemBenizid
	 * @param loanParticipantsDTO the loanParticipants DTO
	 * @return the list
	 */
	List<LoanParticipantsDTO> find(LoanParticipantsDTO loanParticipantsDTO);

	/**
	 * The method used for saving the given {@link LoanParticipantsDTO}.
	 * 
	 * @author HaythemBenizid
	 * @param loanParticipantsDTO the loanParticipants DTO
	 * @return the loanParticipants DTO
	 */
	LoanParticipantsDTO save(LoanParticipantsDTO loanParticipantsDTO);

	/**
	 * THE METHOD USED TO FIND {@link LoanParticipants} AND UPDATE ONLY << DATE_FIN >>.
	 *
	 * @author HaythemBenizid
	 * @param loanParticipantsDTO the loanParticipants DTO
	 * @return the loanParticipants DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	LoanParticipantsDTO update(LoanParticipantsDTO loanParticipantsDTO)
			throws ResourcesNotFoundException;

}
