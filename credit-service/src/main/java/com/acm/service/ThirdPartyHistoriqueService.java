/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.GEDException;
import com.acm.exceptions.type.NationalIdNotFoundException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.ThirdPartyHistoriqueDTO;
import com.acm.utils.enums.ThirdPartyCategory;

/**
 * {@link ThirdPartyHistoriqueService} interface.
 *
 * @author HaythemBenizid
 * @since 1.0.11
 */
public interface ThirdPartyHistoriqueService {

	/**
	 * Find {@link ThirdPartyHistoriqueDTO} by given ID.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @return the thirdPartyHistorique DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	ThirdPartyHistoriqueDTO find(Long id) throws ResourcesNotFoundException;

	/**
	 * Find {@link List} of {@link ThirdPartyHistoriqueDTO} by given params.
	 * 
	 * @author HaythemBenizid
	 * @param thirdPartyHistoriqueDTO the thirdPartyHistorique DTO
	 * @return the list
	 */
	List<ThirdPartyHistoriqueDTO> find(ThirdPartyHistoriqueDTO thirdPartyHistoriqueDTO);

	/**
	 * Find {@link List} of {@link ThirdPartyHistoriqueDTO} : find last stored data for all
	 * {@link ThirdPartyCategory} by id customer.
	 *
	 * @author HaythemBenizid
	 * @param thirdPartyHistoriqueDTO the third party historique DTO
	 * @return the list
	 * @throws NationalIdNotFoundException the national id not found exception
	 */
	List<ThirdPartyHistoriqueDTO> findForScreening(ThirdPartyHistoriqueDTO thirdPartyHistoriqueDTO)
			throws NationalIdNotFoundException;

	/**
	 * The method used for saving the given {@link ThirdPartyHistoriqueDTO}.
	 * 
	 * @author HaythemBenizid
	 * @param thirdPartyHistoriqueDTO the thirdPartyHistorique DTO
	 * @return the thirdPartyHistorique DTO
	 */
	ThirdPartyHistoriqueDTO save(ThirdPartyHistoriqueDTO thirdPartyHistoriqueDTO);

	/**
	 * Find ThirdPartyHistorique by given {@link ThirdPartyCategory} and ID Loan and ID customer for
	 * given category =(CUSTOMER / GUARANTOR).
	 *
	 * @author HaythemBenizid
	 * @param category the category
	 * @param idLoan the id loan
	 * @param idCustomer the id customer
	 * @param categoryCustomer the category customer
	 * @return the list
	 */
	List<ThirdPartyHistoriqueDTO> find(String category, Long idLoan, Long idCustomer,
			String categoryCustomer);

	/**
	 * Generate I-SCORE report Stored in DB After running I-SCORE call.
	 * 
	 * @author HaythemBenizid
	 * @param thirdPartyHistoriqueDTO the third party historique DTO
	 * @return the byte[]
	 */
	byte[] generateIscoreStoredReport(ThirdPartyHistoriqueDTO thirdPartyHistoriqueDTO);

	/**
	 * Generate I-SCORE report : Call I-SCORE API API.
	 *
	 * @author HaythemBenizid
	 * @param thirdPartyHistoriqueDTO the third party historique DTO
	 * @return the byte[]
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws GEDException the GED exception
	 */
	byte[] generateIscoreReport(ThirdPartyHistoriqueDTO thirdPartyHistoriqueDTO)
			throws ResourcesNotFoundException, GEDException;

	/**
	 * The method used for updating the given {@link ThirdPartyHistoriqueDTO} data.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @param thirdPartyHistoriqueDTO the third party historique DTO
	 * @return the third party historique DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	ThirdPartyHistoriqueDTO save(Long id, ThirdPartyHistoriqueDTO thirdPartyHistoriqueDTO)
			throws ResourcesNotFoundException;

	/**
	 * Validate.
	 *
	 * @author Yesser Somai
	 * @param thirdPartyHistoriqueDTO the third party historique DTO
	 * @return the third party historique DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	ThirdPartyHistoriqueDTO validate(ThirdPartyHistoriqueDTO thirdPartyHistoriqueDTO)
			throws ResourcesNotFoundException;

	/**
	 * Find by id loan.
	 *
	 * @param idLoan the id loan
	 * @return the string
	 */
	List<ThirdPartyHistoriqueDTO> findByIdLoan(Long idLoan);

	/**
	 * Find by search query id and category.
	 *
	 * @param searchQueryId the search query id
	 * @param category the category
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	LoanDTO findBySearchQueryIdAndCategory(Long searchQueryId, String category)
			throws ResourcesNotFoundException;

	/**
	 * Find by customer id reis and category.
	 *
	 * @param customerIdReis the customer id reis
	 * @param category the category
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	LoanDTO findByCustomerIdReisAndCategory(Long customerIdReis, String category)
			throws ResourcesNotFoundException;


}
