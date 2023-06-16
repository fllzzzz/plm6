<template>
  <div class="app-container">
    <!--工具栏-->
    <div class="head-container">
      <mHeader>
        <template #optLeft>
          <common-button v-permission="permission.edit" v-show="!isEdit" type="primary" size="mini" @click="isEdit = true">
            编辑
          </common-button>
          <common-button v-show="isEdit" type="success" size="mini" style="margin-left: 0px" @click="previewVisible = true">
            预览并保存
          </common-button>
          <common-button v-show="isEdit" type="info" size="mini" @click="isEdit = false">取消</common-button>
        </template>
      </mHeader>
    </div>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      return-source-data
      :data-format="dataFormat"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      row-key="rowId"
      style="width: 100%"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        v-if="columns.visible('name')"
        key="name"
        prop="name"
        :show-overflow-tooltip="true"
        label="工序名称"
        min-width="140px"
        align="center"
      />
      <!-- <el-table-column
        v-if="columns.visible('wageQuotaTypeStr')"
        key="wageQuotaTypeStr"
        prop="wageQuotaTypeStr"
        label="计价方式"
        min-width="170px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.wageQuotaTypeStr }}</span>
        </template>
      </el-table-column> -->
      <template v-for="item in wageQuotaTypeEnum.ENUM" :key="item.V">
        <el-table-column :label="`${item.L} (${item.unit})`" min-width="170px" align="center">
          <template #default="{ row }">
            <template v-if="!isEdit">
              <span v-if="row.priceMap[item.V]" v-to-fixed="{ k: 'YUAN', val: row.priceMap[item.V] }"></span>
              <span v-else>{{ item.V & row.wageQuotaType ? '未设置' : '/' }}</span>
            </template>
            <template v-else>
              <common-input-number
                v-if="item.V & row.wageQuotaType"
                v-model="row.priceMap[item.V]"
                :precision="decimalPrecision.mes"
                :controls="false"
                size="mini"
                style="width: 100%"
              >
              </common-input-number>
              <span v-else>/</span>
            </template>
          </template>
        </el-table-column>
      </template>
      <!-- <el-table-column v-if="columns.visible('productType')" key="productType" prop="productType" label="类型" align="center" width="110px">
        <template #default="{ row }">
          <el-tag :type="typeEnum.V[row.productType].T">{{ typeEnum.VL[row.productType] }}</el-tag>
        </template>
      </el-table-column> -->
    </common-table>
    <m-preview v-model:visible="previewVisible" :data="crud.data" @saveSuccess="isEdit = false" />
  </div>
</template>

<script setup>
import { getByProductType, editWage } from '@/api/mes/production-config/process'
import { ref } from 'vue'

import { isBlank } from '@data-type/index'
import { wageQuotaTypeMap } from '@/settings/config'
import { wageQuotaTypeEnum } from '@enum-ms/mes'
import { configWageQuotaPM as permission } from '@/page-permission/config'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import mHeader from './module/header'
import mPreview from './module/preview'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

const { decimalPrecision } = useDecimalPrecision()

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const dataFormat = [['wageQuotaTypeStr', ['parse-enum', wageQuotaTypeEnum, { bit: true }], { source: 'wageQuotaType' }]]

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '工序',
    sort: [],
    optShow: { ...optShow },
    permission: { ...permission },
    hasPagination: false,
    crudApi: { get: getByProductType, edit: editWage }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: false })

const previewVisible = ref(false)
const isEdit = ref(false)

function arr2obj(arr, mark = 'id') {
  if (isBlank(arr)) return {}
  const newObj = {}
  for (const item of arr) {
    newObj[item[mark]] = item.price || 0
  }
  return newObj
}

CRUD.HOOK.handleRefresh = (crud, { data }) => {
  data.content = data.map((v, i) => {
    v.rowId = i + '' + Math.random()
    v.wageQuotaType = wageQuotaTypeMap[v.productType]
    v.priceMap = arr2obj(v.processWageList, 'wageQuotaType')
    v.originPriceMap = arr2obj(v.processWageList, 'wageQuotaType')
    return v
  })
}
</script>
