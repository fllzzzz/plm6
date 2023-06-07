<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <common-radio-button
        v-model="query.boolStatus"
        showOptionAll
        :options="enabledEnum.ENUM"
        class="filter-item"
        type="enum"
        @change="crud.toQuery"
      />
      <el-input
        v-model.trim="query.name"
        clearable
        style="width: 150px"
        size="small"
        placeholder="输入设备名称搜索"
        class="filter-item"
        @keyup.enter="crud.toQuery"
      />
      <rrOperation />
    </div>
    <crudOperation>
      <template #optRight>
        <upload-btn
          v-if="checkPermission(crud.permission.import)"
          :upload-fun="listUpload"
          btn-name="导入设备清单"
          btn-type="success"
          btn-size="mini"
          class="filter-item"
          @success="crud.toQuery"
        />
        <export-button v-if="checkPermission(crud.permission.download)" :fn="downloadTemplate" class="filter-item">
          导出模板
        </export-button>
      </template>
      <template #viewLeft>
        <print-table v-permission="crud.permission.print" api-key="contractDeviceDepreciationRecord" size="mini" type="warning" />
      </template>
    </crudOperation>
  </div>
</template>
<script setup>
import { listUpload, downloadTemplate } from '@/api/contract/expense-entry/device-depreciation'

import { enabledEnum } from '@enum-ms/common'
import checkPermission from '@/utils/system/check-permission'

import crudOperation from '@crud/CRUD.operation'
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import uploadBtn from '@comp/file-upload/ExcelUploadBtn'
import ExportButton from '@comp-common/export-button/index.vue'

const defaultQuery = {
  boolStatus: enabledEnum.TRUE.V,
  name: undefined
}

const { crud, query } = regHeader(defaultQuery)
</script>
