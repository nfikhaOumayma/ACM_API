/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.exceptions.type.RequestAlreadyExistException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.ExceptionRequestService;
import com.acm.utils.dtos.ExceptionRequestCountDTO;
import com.acm.utils.dtos.ExceptionRequestDTO;
import com.acm.utils.dtos.pagination.ExceptionRequestPaginationDTO;

/**
 * {@link ExceptionRequestController } class.
 *
 * @author ManelLamloum
 * @since 0.1.0
 */
@RestController
@RequestMapping("/exception-request")
public class ExceptionRequestController {

	/** The exception request service. */
	@Autowired
	private ExceptionRequestService exceptionRequestService;

	/**
	 * Find.
	 * 
	 * @author ManelLamloum
	 * @param exceptionRequestDTO the exception request DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<ExceptionRequestDTO> find(@RequestBody ExceptionRequestDTO exceptionRequestDTO) {

		return exceptionRequestService.find(exceptionRequestDTO);
	}

	/**
	 * Adds the.
	 * 
	 * @author ManelLamloum
	 * @param exceptionRequestDTO the exception request DTO
	 * @return the exception request DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws RequestAlreadyExistException the request already exist exception
	 */
	@PostMapping("/save")
	public ExceptionRequestDTO add(@RequestBody ExceptionRequestDTO exceptionRequestDTO)
			throws ResourcesNotFoundException, RequestAlreadyExistException {

		return exceptionRequestService.save(exceptionRequestDTO);
	}

	/**
	 * Update.
	 * 
	 * @author ManelLamloum
	 * @param exceptionRequestDTO the exception request DTO
	 * @return the exception request DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/update")
	public ExceptionRequestDTO update(@RequestBody ExceptionRequestDTO exceptionRequestDTO)
			throws ResourcesNotFoundException {

		return exceptionRequestService.save(exceptionRequestDTO.getId(), exceptionRequestDTO);
	}

	/**
	 * Find.
	 * 
	 * @author ManelLamloum
	 * @param exceptionRequestPaginationDTO the exception request pagination DTO
	 * @return the exception request pagination DTO
	 */
	@PostMapping("/find-pagination")
	public ExceptionRequestPaginationDTO find(
			@RequestBody ExceptionRequestPaginationDTO exceptionRequestPaginationDTO) {

		return exceptionRequestService.findPagination(exceptionRequestPaginationDTO);
	}

	/**
	 * Update status and assign to user connected.
	 *
	 * @param exceptionRequestDTO the exception request DTO
	 * @return the exception request DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/update-status")
	public ExceptionRequestDTO updateStatusAndAssignToUserConnected(
			@RequestBody ExceptionRequestDTO exceptionRequestDTO)
			throws ResourcesNotFoundException {

		return exceptionRequestService.updateStatusAndAssignToUserConnected(exceptionRequestDTO);
	}

	/**
	 * Count tabs.
	 * 
	 * @author ManelLamloum
	 * @return the exception request count DTO
	 */
	@GetMapping("/count-tabs")
	public ExceptionRequestCountDTO countTabNew() {

		return exceptionRequestService.count();
	}
}
