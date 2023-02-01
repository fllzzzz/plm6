<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
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
        v-model="query.plateType"
        placeholder="输入板型搜索"
        class="filter-item"
        style="width: 200px;"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <rrOperation/>
    </div>
    <crudOperation>
      <template v-if="query.monomerId" #optRight>
        <span v-if="checkPermission(crud.permission.save)" style="margin-right: 6px">
          <span v-if="modifying">
            <common-button type="warning" size="mini" @click="handelModifying(false, true)">取消录入</common-button>
            <common-button type="success" size="mini" @click="previewVisible = true">预览并保存</common-button>
          </span>
          <common-button v-else type="primary" size="mini" @click="handelModifying(true)">录入价格</common-button>
        </span>
        <print-table
          v-permission="crud.permission.print"
          api-key="contractEnclosurePrice"
          :params="{ monomerId: query.monomerId }"
          size="mini"
          type="warning"
          class="filter-item"
        />
      </template>
      <template #viewLeft>
        <span v-if="checkPermission(crud.permission.cost) && query.monomerId">
          <el-tag effect="plain" type="success" size="medium" class="filter-item">
            单体围护总数(张)：
            <span v-if="!costLoading">{{ monomerCost.totalQuantity }}</span>
            <i v-else class="el-icon-loading" />
          </el-tag>
          <el-tag effect="plain" type="success" size="medium" class="filter-item">
            单体围护总长(m)：
            <span v-if="!costLoading">{{ convertUnits(monomerCost.totalLength, 'mm', 'm', DP.MES_ENCLOSURE_L__M) }}</span>
            <i v-else class="el-icon-loading" />
          </el-tag>
          <el-tag effect="plain" type="success" size="medium" class="filter-item">
            单体围护面积(㎡)：
            <span v-if="!costLoading">{{ convertUnits(monomerCost.totalArea, 'mm2','m2', DP.COM_AREA__M2) }}</span>
            <i v-else class="el-icon-loading" />
          </el-tag>
          <el-tag effect="plain" type="success" size="medium" class="filter-item">
            单体围护造价：
            <span v-if="!costLoading" v-thousand="monomerCost.totalPrice" />
            <i v-else class="el-icon-loading" />
          </el-tag>
        </span>
      </template>
    </crudOperation>
    <mPreview v-model="previewVisible" :modified-data="modifiedData" v-bind="$attrs" :params="previewParams" @success="handleSuccess" />
  </div>
</template>

<script setup>
import { cost } from '@/api/contract/sales-manage/price-manage/enclosure'
import { ref, watch, nextTick, inject, computed, defineExpose } from 'vue'

import checkPermission from '@/utils/system/check-permission'
import { convertUnits } from '@/utils/convert/unit'
import { DP } from '@/settings/config'
// import { packTypeEnum } from '@enum-ms/mes'
import { enclosureSettlementTypeEnum } from '@enum-ms/contract'
import { toThousand } from '@/utils/data-type/number'
import { emptyTextFormatter } from '@/utils/data-type'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import mPreview from '../../preview'

const projectId = inject('projectId')
const monomerId = inject('monomerId')

// 有变动的数据
const modifiedData = computed(() => {
  return crud.data.filter((v) => v.unitPrice !== v.originUnitPrice)
})

// 预览参数
const previewParams = computed(() => {
  return {
    monomerId: query.monomerId
    // type: packTypeEnum.ENCLOSURE.V
  }
})

watch(
  monomerId,
  (val) => {
    nextTick(() => {
      crud.query.monomerId = val
      costInit()
      crud.toQuery()
    })
  },
  { immediate: true }
)

const modifying = ref(false)
const costLoading = ref(false)
const previewVisible = ref(false)
const costData = {
  totalArea: 0,
  totalAreaPrice: 0,
  totalLength: 0,
  totalLengthPrice: 0,
  totalQuantity: 0
}
const monomerCost = ref({ ...costData })

const defaultQuery = {
  name: undefined, plateType: undefined,
  monomerId: { value: undefined, resetAble: false }
}
const { crud, query, CRUD } = regHeader(defaultQuery)

// 刷新数据后
CRUD.HOOK.handleRefresh = (crud, { data }) => {
  data.content.forEach(v => {
    v.newUnitPrice = v.unitPrice // number类型的单价（unitPrice可能会有千位符）
    v.originNewUnitPrice = v.newUnitPrice
    v.originUnitPrice = emptyTextFormatter(toThousand(v.unitPrice))
    v.totalLength = convertUnits(v.totalLength, 'mm', 'm', DP.MES_ENCLOSURE_L__M)
    v.totalArea = convertUnits(v.totalArea, 'mm2', 'm2', DP.COM_AREA__M2)
    v.totalPrice = (v.priceType === enclosureSettlementTypeEnum.LENGTH.V ? v.totalLength : v.totalArea) * (v.unitPrice || 0)
  })
  fetchCost()
}

// 获取商务围护成本
async function fetchCost() {
  if (!checkPermission(crud.permission.cost)) return
  costLoading.value = true
  try {
    const res = await cost({
      monomerId: query.monomerId,
      projectId: projectId.value
    })
    monomerCost.value = res
  } catch (error) {
    console.log('获取商务围护成本失败', error)
  } finally {
    costLoading.value = false
  }
}

// 成本初始化
function costInit() {
  monomerCost.value = { ...costData }
}

// 出来录入状态
function handelModifying(status, reset = false) {
  // 取消分配，数据还原
  if (reset) {
    crud.data.forEach((v) => {
      v.unitPrice = v.originUnitPrice
      v.newUnitPrice = v.originNewUnitPrice
      v.totalPrice = (v.priceType === enclosureSettlementTypeEnum.LENGTH.V ? v.totalLength : v.totalArea) * (v.newUnitPrice || 0)
      return v
    })
  }
  modifying.value = status
}

// 提交成功后
function handleSuccess() {
  modifying.value = false
  crud.toQuery()
}

defineExpose({
  modifying
})
</script>
