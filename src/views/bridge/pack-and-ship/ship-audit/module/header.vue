<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <project-radio-button size="small" v-model="query.projectId" class="filter-item" @change="crud.toQuery" />
      <common-radio-button
        v-model="query.checkStatus"
        :options="shipAuditStatusEnum.ENUM"
        showOptionAll
        type="enum"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      />
      <component-radio-button
        v-model="query.productType"
        :options="packTypeEnum.ENUM"
        showOptionAll
        type="enum"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      />
      <div>
        <el-input
          v-model="query.serialNumber"
          placeholder="可输入车次搜索"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
          @keyup.enter="crud.toQuery"
        />
        <el-input
          v-model="query.licensePlate"
          placeholder="可输入车牌搜索"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
          @keyup.enter="crud.toQuery"
        />
        <el-input
          v-model="query.userName"
          placeholder="可输入装车人搜索"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
          @keyup.enter="crud.toQuery"
        />
        <el-input
          v-model="query.actualUserName"
          placeholder="可输入过磅人搜索"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
          @keyup.enter="crud.toQuery"
        />
        <rrOperation />
      </div>
    </div>
    <crudOperation>
      <template v-slot:optLeft>
        <print-table
          v-permission="crud.permission.print"
          api-key="bridgeShipmentAudit"
          :params="{ ...query }"
          size="mini"
          type="warning"
          class="filter-item"
        />
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { bridgePackTypeEnum as packTypeEnum } from '@enum-ms/bridge'
import { shipAuditStatusEnum } from '@enum-ms/mes'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  serialNumber: undefined,
  licensePlate: undefined,
  userName: undefined,
  actualUserName: undefined,
  projectId: { value: undefined, resetAble: false },
  productType: { value: undefined, resetAble: false }
}
const { crud, query } = regHeader(defaultQuery)

</script>
