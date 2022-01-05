<template>
  <div class="app-container">
    <!--工具栏-->
    <div class="head-container">
      <mHeader ref="headRef" />
    </div>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      style="width: 100%"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        v-if="columns.visible('productProcessName') && crud.query.productType !== typeEnum.MACHINE_PART.V"
        key="productProcessName"
        prop="productProcessName"
        :show-overflow-tooltip="true"
        label="名称"
        width="140px"
      />
      <el-table-column
        v-if="columns.visible('processSequence')"
        key="processSequence"
        prop="processSequence"
        :show-overflow-tooltip="true"
        label="【工序 │ 单价】"
        min-width="160px"
      >
        <template v-slot="scope">
          <span v-for="(item, index) in scope.row.processSequence" :key="item.processId">
            <span style="cursor: pointer" v-html="item.html" @click="item.price && crud.toEdit(item)" />
            <span v-if="index !== scope.row.processSequence.length - 1">→</span>
          </span>
        </template>
      </el-table-column>
      <el-table-column
        align="right"
        :label="`合计:${crud.query.productType === typeEnum.ENCLOSURE.V ? '（元/米）' : '（元/吨）'}`"
        width="120"
        fixed="right"
      >
        <template v-slot="scope">
          <span>{{ scope.row.totalPrice }}</span>
        </template>
      </el-table-column>
    </common-table>
    <mForm />
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/team-report/wages-adjust'
import { ref, provide } from 'vue'

import { isNotBlank } from '@data-type/index'
import { wageQuotaTypeEnum, processMaterialListTypeEnum as typeEnum } from '@enum-ms/mes'
import { deepClone } from '@data-type/index'
import { mapGetters } from '@/store/lib'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import mHeader from './module/header'
import mForm from './module/form'

// crud交由presenter持有
const permission = {
  get: ['wagesAdjust:get'],
  edit: ['wagesAdjust:edit'],
  audit: ['wagesAdjust:audit']
}

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}
const { globalProjectId } = mapGetters('globalProjectId')
provide('projectId', globalProjectId)

const headRef = ref()
const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '工价调整',
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: false,
    requiredQuery: ['monomerId']
  },
  tableRef
)
const { maxHeight } = useMaxHeight({ paginate: false })
provide('query', crud.query)

CRUD.HOOK.handleRefresh = (crud, res) => {
  const monomer = headRef.value?.getMonomer() || {}
  res.data.content = res.data.content.map((v) => {
    v.process = deepClone(v.processList)
    if (v.process && v.process.length > 0) {
      v.processOption = {}
      v.totalPrice = 0
      v.productProcessId = v.productProcessId || 0// 零件没有总工序默认0
      v.process.forEach((o) => {
        v.processOption[o.processId] = {
          id: o.processId,
          price: o.price,
          name: o.processName,
          wageQuotaType: o.wageQuotaType
          // wageQuota: o.wageQuota,
          // productProcessId: o.productProcessId
        }
        v.totalPrice += o.price || 0
      })
      v.processSequence = v.process.map((o) => {
        const unit = isNotBlank(o.wageQuotaType) ? wageQuotaTypeEnum.V[o.wageQuotaType].unit : ''
        return {
          monomerId: crud.query.monomerId,
          monomerName: monomer.name,
          projectId: crud.query.projectId,
          productType: crud.query.productType,
          productProcessId: v.productProcessId,
          name: v.productProcessName,
          ...deepClone(o),
          originPrice: o.price,
          html: `<span>【${o.processName} │ <span style="color: #67C23A;">${o.price} ${unit}</span>】</span>`
        }
      })
      v.processSequenceIds = v.process.map((v) => v.processId)
    } else {
      v.processSequence = ''
      v.processSequenceIds = []
    }
    return v
  })
}
</script>
