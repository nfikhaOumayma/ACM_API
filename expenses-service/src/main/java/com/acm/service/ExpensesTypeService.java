/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ExpensesTypeUnicityCodeException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.ExpensesTypeDTO;

/**
 * {@link ExpensesTypeService} class.
 *
 * @author YesserSomai
 * @since 1.1.3
 */
public interface ExpensesTypeService {

	/**
	 * Find all Expenses Type.
	 *
	 * @return the list
	 */
	List<ExpensesTypeDTO> findAll();

	/**
	 * Save.
	 *
	 * @param expensesTypeDTO the expenses type DTO
	 * @return the expenses type DTO
	 * @throws ExpensesTypeUnicityCodeException the expenses type unicity code exception
	 */
	ExpensesTypeDTO save(ExpensesTypeDTO expensesTypeDTO) throws ExpensesTypeUnicityCodeException;

	/**
	 * Save Expenses Type.
	 *
	 * @author YesserSomai
	 * @param id the id
	 * @param expensesTypeDTO the expenses type DTO
	 * @return the customer contact DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	ExpensesTypeDTO save(Long id, ExpensesTypeDTO expensesTypeDTO)
			throws ResourcesNotFoundException;

	/**
	 * Delete Expenses Type.
	 *
	 * @author YesserSomai
	 * @param id the id
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	void delete(Long id) throws ResourcesNotFoundException;

	/**
	 * Find expenses type by id.
	 * 
	 * @author idridi
	 * @param id the id
	 * @return the expenses type DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	ExpensesTypeDTO findExpensesTypeById(Long id) throws ResourcesNotFoundException;

	/**
	 * Update document name.
	 *
	 * @param documentLibel the document libel
	 * @param documentId the document id
	 */
	void updateDocumentName(String documentLibel, Long documentId);

	/**
	 * Find.
	 *
	 * @param expensesTypeDTO the expenses type DTO
	 * @return the list
	 */
	List<ExpensesTypeDTO> find(ExpensesTypeDTO expensesTypeDTO);

}
