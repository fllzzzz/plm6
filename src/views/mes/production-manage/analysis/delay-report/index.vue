<template>
  <div class="app-container">
    <div class="head-container">
      <mHeader />
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
        v-if="columns.visible('askCompleteTime')"
        key="askCompleteTime"
        prop="askCompleteTime"
        label="完成节点"
        align="center"
        min-width="70px"
      >
        <template v-slot="scope">
          <span v-parse-time="'{y}-{m}-{d}'">{{ scope.row.askCompleteTime }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('taskQuantity')"
        key="taskQuantity"
        prop="taskQuantity"
        label="计划量"
        align="center"
        min-width="100px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.taskQuantity }} / {{ scope.row.taskMete }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('completeQuantity')"
        key="completeQuantity"
        prop="completeQuantity"
        label="完成量"
        align="center"
        min-width="100px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.completeQuantity }} / {{ scope.row.completeMete }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('diffQuantity')"
        key="diffQuantity"
        prop="diffQuantity"
        label="差异"
        align="center"
        min-width="100px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.diffQuantity }} / {{ scope.row.diffMete }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('completeRate')"
        key="completeRate"
        prop="completeRate"
        label="完成率"
        align="center"
        min-width="70px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.completeRate }}</span>
        </template>
      </el-table-column>
      <el-table-column v-permission="permission.detail" label="操作" width="100px" align="center" fixed="right">
        <template v-slot="scope">
          <common-button size="mini" type="info" @click="toDetail(scope.row)">查看</common-button>
        </template>
      </el-table-column>
    </common-table>
    <mDetail v-model:visible="detailVisible" :info="itemInfo"></mDetail>
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/production-manage/analysis/delay-report'
import { ref, provide } from 'vue'

import { DP } from '@/settings/config'
import { toFixed } from '@data-type/index'
import { convertUnits } from '@/utils/convert/unit'
import { reportComponentTypeEnum } from '@enum-ms/mes'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import mHeader from './module/header'
import mDetail from './module/detail'

// crud交由presenter持有
const permission = {
  get: ['analysisDelayReport:get'],
  detail: ['analysisDelayReport:detail']
}

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '迟滞报表',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    dataPath: '',
    hasPagination: false
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: false })

provide('query', crud.query)
const detailVisible = ref(false)
const itemInfo = ref({})

function toDetail(row) {
  itemInfo.value = row
  detailVisible.value = true
}

CRUD.HOOK.handleRefresh = (crud, res) => {
  if (crud.query.componentType === reportComponentTypeEnum.ARTIFACT.V) {
    res.data = res.data.artifactAssembleList.map((v) => {
      v.taskMete = toFixed(v.taskNetWeight, DP.COM_WT__KG) || 0
      v.completeMete = toFixed(v.completeNetWeight, DP.COM_WT__KG) || 0
      v.diffQuantity = v.taskQuantity - v.completeQuantity || 0
      v.diffMete = toFixed(v.taskNetWeight - v.completeNetWeight, DP.COM_WT__KG)
      v.completeRate = Number(v.taskMete) ? toFixed((Number(v.completeMete) / Number(v.taskMete)) * 100, 2) + '%' : '0%'
      return v
    })
  } else {
    res.data = res.data.enclosureList.map((v) => {
      v.taskMete = convertUnits(v.taskLength, 'mm', 'm', DP.MES_ENCLOSURE_L__M) || 0
      v.completeMete = convertUnits(v.completeLength, 'mm', 'm', DP.MES_ENCLOSURE_L__M) || 0
      v.diffQuantity = v.taskQuantity - v.completeQuantity || 0
      v.diffMete = convertUnits(v.taskLength - v.completeLength, 'mm', 'm', DP.MES_ENCLOSURE_L__M)
      v.completeRate = Number(v.taskMete) ? toFixed((Number(v.completeMete) / Number(v.taskMete)) * 100, 2) + '%' : '0%'
      return v
    })
  }
}
</script>
