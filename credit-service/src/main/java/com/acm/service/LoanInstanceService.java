/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.LoanInstanceDTO;

/**
 * {@link LoanInstanceService} interface.
 *
 * @author HaythemBenizid
 * @since 0.4.0
 */
public interface LoanInstanceService {

	/**
	 * Find {@link List} of {@link LoanInstanceDTO} by given params. using Querydsl.
	 *
	 * @author HaythemBenizid
	 * @param loanInstanceDTO the loan instance DTO
	 * @return the list
	 */
	List<LoanInstanceDTO> find(LoanInstanceDTO loanInstanceDTO);

	/**
	 * The method used for saving the given {@link LoanInstanceDTO}.
	 *
	 * @author HaythemBenizid
	 * @param loanInstanceDTO the loanInstance DTO
	 * @return the loanInstance DTO
	 */
	LoanInstanceDTO save(LoanInstanceDTO loanInstanceDTO);

	/**
	 * The method used for updating the workflow process of the given loan if the approval amount
	 * was changed.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	List<LoanInstanceDTO> updateForWorkflow(LoanDTO loanDTO) throws ResourcesNotFoundException;

	/**
	 * The method used for updating the given {@link LoanInstanceDTO} by ID.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @param loanInstanceDTO the loanInstance DTO
	 * @return the loanInstance DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	LoanInstanceDTO save(Long id, LoanInstanceDTO loanInstanceDTO)
			throws ResourcesNotFoundException;

}
