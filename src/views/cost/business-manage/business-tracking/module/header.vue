<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <common-radio-button
        v-model="query.productType"
        :options="packTypeEnum.ENUM"
        showOptionAll
        type="enum"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      />
      <monomer-select
        ref="monomerRef"
        v-model="query.monomerId"
        clearable
        :default="false"
        :project-id="globalProjectId"
        :filterArea="false"
        class="filter-item"
        @change="crud.toQuery"
      />
      <el-date-picker
        v-model="query.deliveryDate"
        type="daterange"
        range-separator=":"
        size="small"
        class="filter-item date-item"
        start-placeholder="开始日期"
        end-placeholder="结束日期"
        style="width:240px"
        :shortcuts="PICKER_OPTIONS_SHORTCUTS"
        @change="handleDateChange"
      />
      <div>
        <el-input
          v-model="query.name"
          placeholder="可输入名称搜索"
          class="filter-item"
          style="width: 200px;"
          size="small"
          clearable
          @keyup.enter="crud.toQuery"
        />
        <el-input
          v-model="query.serialNumber"
          placeholder="可输入编号搜索"
          class="filter-item"
          style="width: 200px;"
          size="small"
          clearable
          @keyup.enter="crud.toQuery"
        />
        <el-input
          v-model="query.specification"
          placeholder="可输入规格搜索"
          class="filter-item"
          style="width: 200px;"
          size="small"
          clearable
          @keyup.enter="crud.toQuery"
        />
        <el-input
          v-model="query.material"
          placeholder="可输入材质搜索"
          class="filter-item"
          style="width: 200px;"
          size="small"
          clearable
          @keyup.enter="crud.toQuery"
        />
        <rrOperation/>
      </div>
    </div>
    <crudOperation>
      <template #viewLeft>
      <el-tag type="success" v-loading="businessLoading" effect="plain" size="medium">总价款： <span v-thousand="businessInfo.totalPrice" /></el-tag>
      <el-tag type="success" v-loading="businessLoading" style="margin-left: 6px" effect="plain" size="medium">累计进度额：<span v-thousand="businessInfo.collection" /></el-tag>
      <el-tag type="success" v-loading="businessLoading" style="margin-left: 6px" effect="plain" size="medium">完成率：{{businessInfo.rate}}%</el-tag>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { ref } from 'vue'
import { summary } from '@/api/cost/business-manage/business-tracking'
import moment from 'moment'

import { packTypeEnum } from '@enum-ms/mes'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'
import { mapGetters } from '@/store/lib'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import monomerSelect from '@/components-system/plan/monomer-select'

const defaultQuery = {
  serialNumber: undefined, name: undefined,
  material: undefined, specification: undefined,
  auditStartDate: undefined, auditEndDate: undefined,
  productType: { value: undefined, resetAble: false },
  projectId: { value: undefined, resetAble: false },
  monomerId: { value: undefined, resetAble: false }
}
const { crud, query, CRUD } = regHeader(defaultQuery)
const { globalProjectId } = mapGetters(['globalProjectId'])

const businessLoading = ref(false)
const businessInfo = ref({
  totalPrice: 10000,
  collection: 6000,
  rate: 60
})

CRUD.HOOK.beforeToQuery = () => {
  query.projectId = globalProjectId
  fetchBusinessInfo()
}

// 获取项目汇总数据
async function fetchBusinessInfo() {
  businessLoading.value = true
  try {
    businessInfo.value = (await summary({ ...query })) || {}
    businessInfo.value.rate = businessInfo.value.collection / businessInfo.value.totalPrice * 100 || 0
  } catch (error) {
    console.log('获取商务汇总数据', error)
  } finally {
    businessLoading.value = false
  }
}

function handleDateChange() {
  if (query.deliveryDate && query.deliveryDate.length > 1) {
    query.auditStartDate = moment(query.deliveryDate[0]).valueOf()
    query.auditEndDate = moment(query.deliveryDate[1]).valueOf()
  } else {
    query.auditStartDate = undefined
    query.auditEndDate = undefined
  }
  crud.toQuery()
}
</script>
