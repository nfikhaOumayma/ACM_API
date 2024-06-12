/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.RequestAlreadyExistException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.ExceptionRequestCountDTO;
import com.acm.utils.dtos.ExceptionRequestDTO;
import com.acm.utils.dtos.pagination.ExceptionRequestPaginationDTO;

/**
 * {@link ExceptionRequestService } class.
 *
 * @author ManelLamloum
 * @since 0.1.0
 */
public interface ExceptionRequestService {

	/**
	 * Find.
	 * 
	 * @author ManelLamloum
	 * @param exceptionRequestDTO the exception request DTO
	 * @return the list
	 */
	List<ExceptionRequestDTO> find(ExceptionRequestDTO exceptionRequestDTO);

	/**
	 * Find.
	 * 
	 * @author ManelLamloum
	 * @param exceptionRequestPaginationDTO the exception request pagination DTO
	 * @return the list
	 */
	ExceptionRequestPaginationDTO findPagination(
			ExceptionRequestPaginationDTO exceptionRequestPaginationDTO);

	/**
	 * Save.
	 * 
	 * @author ManelLamloum
	 * @param exceptionRequestDTO the exception request DTO
	 * @return the exception request DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws RequestAlreadyExistException the request already exist exception
	 */
	ExceptionRequestDTO save(ExceptionRequestDTO exceptionRequestDTO)
			throws ResourcesNotFoundException, RequestAlreadyExistException;

	/**
	 * Save.
	 *
	 * @author ManelLamloum
	 * @param id the id
	 * @param exceptionRequestDTO the exception request DTO
	 * @return the exception request DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	ExceptionRequestDTO save(Long id, ExceptionRequestDTO exceptionRequestDTO)
			throws ResourcesNotFoundException;

	/**
	 * Update status and assign to user connected.
	 * 
	 * @author ManelLamloum
	 * @param exceptionRequestDTO the exception request DTO
	 * @return the exception request DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	ExceptionRequestDTO updateStatusAndAssignToUserConnected(
			ExceptionRequestDTO exceptionRequestDTO) throws ResourcesNotFoundException;

	/**
	 * Count.
	 * 
	 * @author ManelLamloum
	 * @return the exception request count DTO
	 */
	ExceptionRequestCountDTO count();
}
