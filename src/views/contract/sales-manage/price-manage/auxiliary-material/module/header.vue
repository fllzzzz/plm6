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
        v-model="query.specification"
        placeholder="输入规格搜索"
        class="filter-item"
        style="width: 200px;"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <el-input
        v-model="query.serialNumber"
        placeholder="输入编号搜索"
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
          api-key="contractAuxiliaryMaterialPrice"
          :params="{ monomerId: query.monomerId }"
          size="mini"
          type="warning"
          class="filter-item"
        />
      </template>
      <template #viewLeft>
        <span v-if="checkPermission(crud.permission.cost) && query.monomerId">
          <el-tag effect="plain" type="success" size="medium" class="filter-item">
            单体配套件总量：
            <span v-if="!costLoading">{{ monomerCost.totalMete }}</span>
            <i v-else class="el-icon-loading" />
          </el-tag>
          <el-tag effect="plain" type="success" size="medium">
            单体配套件造价(元)：
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
import { cost } from '@/api/contract/sales-manage/price-manage/auxiliary-material'
import { ref, watch, nextTick, inject, computed, defineExpose } from 'vue'

import checkPermission from '@/utils/system/check-permission'
import { packTypeEnum } from '@enum-ms/mes'
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
    monomerId: query.monomerId,
    type: packTypeEnum.AUXILIARY_MATERIAL.V
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
  totalPrice: 0,
  totalMete: 0
}
const monomerCost = ref({ ...costData })

const defaultQuery = {
  name: undefined, specification: undefined, serialNumber: undefined,
  monomerId: { value: undefined, resetAble: false }
}
const { crud, query, CRUD } = regHeader(defaultQuery)

// 刷新数据后
CRUD.HOOK.handleRefresh = (crud, { data }) => {
  data.content.forEach(v => {
    v.newUnitPrice = v.unitPrice // number类型的单价（unitPrice可能会有千位符）
    v.originNewUnitPrice = v.newUnitPrice
    v.originUnitPrice = emptyTextFormatter(toThousand(v.unitPrice))
    v.totalPrice = v.mete * (v.unitPrice || 0)
  })
  fetchCost()
}

// 获取商务配套件成本
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
    console.log('获取商务配套件成本失败', error)
  } finally {
    costLoading.value = false
  }
}

// 成本初始化
function costInit() {
  monomerCost.value = { ...costData }
}

// 处理录入状态
function handelModifying(status, reset = false) {
  // 取消分配，数据还原
  if (reset) {
    crud.data.forEach((v) => {
      v.unitPrice = v.originUnitPrice
      v.newUnitPrice = v.originNewUnitPrice
      if (typeof v.newUnitPrice === 'string') {
        v.totalPrice = 0
      } else {
        v.totalPrice = v.mete * (v.newUnitPrice || 0)
      }
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
