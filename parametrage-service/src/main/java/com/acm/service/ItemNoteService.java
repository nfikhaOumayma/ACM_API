/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.ItemNoteDTO;

/**
 * The Interface CollectionNoteService.
 */
public interface ItemNoteService {

	/**
	 * Find.
	 *
	 * @param itemNoteDTO the item note DTO
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	List<ItemNoteDTO> find(ItemNoteDTO itemNoteDTO) throws ResourcesNotFoundException;

	/**
	 * Save.
	 *
	 * @param itemNoteDTO the item note DTO
	 * @return the collection note DTO
	 */
	ItemNoteDTO save(ItemNoteDTO itemNoteDTO);

}
