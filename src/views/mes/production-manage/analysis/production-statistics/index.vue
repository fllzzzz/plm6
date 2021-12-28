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
      :span-method="spanMethod"
      :stripe="false"
      style="width: 100%"
    >
      <el-table-column v-if="columns.visible('beginMete')" key="beginMete" prop="beginMete" label="期初量" align="center" min-width="70px">
        <template v-slot="scope">
          <span>{{ scope.row.beginMete }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('taskMete')" key="taskMete" prop="taskMete" label="本月分配" align="center" min-width="70px">
        <template v-slot="scope">
          <span>{{ scope.row.taskMete }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('totalMete')" key="totalMete" prop="totalMete" label="总量" align="center" min-width="70px">
        <template v-slot="scope">
          <span>{{ scope.row.totalMete }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('completeMete')"
        key="completeMete"
        prop="completeMete"
        label="已完成"
        align="center"
        min-width="70px"
      >
        <template v-slot="scope">
          <span class="tc-success">{{ scope.row.completeMete }}</span>
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
          <span class="tc-success">{{ scope.row.completeRate }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('inProductionMete')"
        key="inProductionMete"
        prop="inProductionMete"
        label="在制品"
        align="center"
        min-width="70px"
      >
        <template v-slot="scope">
          <span class="tc-warning">{{ scope.row.inProductionMete }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('inProductionRate')"
        key="inProductionRate"
        prop="inProductionRate"
        label="在制品率"
        align="center"
        min-width="70px"
      >
        <template v-slot="scope">
          <span class="tc-warning">{{ scope.row.inProductionRate }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('unProducedMete')"
        key="unProducedMete"
        prop="unProducedMete"
        label="未生产"
        align="center"
        min-width="70px"
      >
        <template v-slot="scope">
          <span class="tc-danger">{{ scope.row.unProducedMete }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('unProducedRate')"
        key="unProducedRate"
        prop="unProducedRate"
        label="未生产率"
        align="center"
        min-width="70px"
      >
        <template v-slot="scope">
          <span class="tc-danger">{{ scope.row.unProducedRate }}</span>
        </template>
      </el-table-column>
      <el-table-column label="操作" width="100px" align="center" fixed="right">
        <template v-slot="scope">
          <common-button size="mini" type="info" @click="toDetail(scope.row)">查看</common-button>
        </template>
      </el-table-column>
    </common-table>
    <artifact-detail v-model:visible="artifactDetailVisible"></artifact-detail>
    <enclosure-detail v-model:visible="enclosureDetailVisible"></enclosure-detail>
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/production-manage/analysis/production-statistics'
import { ref, provide } from 'vue'

import { DP } from '@/settings/config'
import { toFixed } from '@data-type/index'
import { convertUnits } from '@/utils/convert/unit'
import { reportComponentTypeEnum } from '@enum-ms/mes'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import mHeader from './module/header'
import artifactDetail from './module/artifact-group-detail'
import enclosureDetail from './module/enclosure-group-detail'

// crud交由presenter持有
const permission = {
  get: [''],
  edit: [''],
  add: [''],
  del: ['']
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
    title: '生产统计',
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

function spanMethod({ row, column, rowIndex, columnIndex }) {
  if ([4, 6, 8, 9].includes(columnIndex)) {
    if (rowIndex === 0) {
      return {
        rowspan: 2,
        colspan: 1
      }
    } else {
      return {
        rowspan: 0,
        colspan: 0
      }
    }
  }
}

CRUD.HOOK.handleRefresh = (crud, res) => {
  if (crud.query.productType === reportComponentTypeEnum.ARTIFACT.V) {
    const _data = res.data['artifactAnalysis']
    const beginMete = toFixed(_data.surplusNetWeight, DP.COM_WT__KG)
    const taskMete = toFixed(_data.taskNetWeight, DP.COM_WT__KG)
    const totalMete = toFixed(_data.surplusNetWeight + _data.taskNetWeight, DP.COM_WT__KG)
    const completeMete = toFixed(_data.completeNetWeight, DP.COM_WT__KG)
    const inProductionMete = toFixed(_data.inProductionNetWeight, DP.COM_WT__KG)
    const unProducedMete = toFixed(
      _data.surplusNetWeight + _data.taskNetWeight - _data.completeNetWeight - _data.inProductionNetWeight,
      DP.COM_WT__KG
    )
    // const totalQuantity = _data.surplusTaskQuantity + _data.taskQuantity
    const completeRate = Number(completeMete) ? toFixed((completeMete / totalMete) * 100, 2) + '%' : '0%'
    const inProductionRate = Number(inProductionMete) ? toFixed((inProductionMete / totalMete) * 100, 2) + '%' : '0%'
    // const unProducedQuantity = totalQuantity - _data.completeQuantity - _data.inProductionQuantity
    const unProducedRate = Number(unProducedMete) ? toFixed((unProducedMete / totalMete) * 100, 2) + '%' : '0%'
    res.data = [
      // {
      //   beginMete: _data.surplusTaskQuantity,
      //   taskMete: _data.taskQuantity,
      //   totalMete: totalQuantity,
      //   completeMete: _data.completeQuantity,
      //   inProductionMete: _data.inProductionQuantity,
      //   unProducedMete: unProducedQuantity
      // },
      { beginMete, taskMete, totalMete, completeRate, inProductionRate, completeMete, inProductionMete, unProducedMete, unProducedRate }
    ]
  }
  if (crud.query.productType === reportComponentTypeEnum.ENCLOSURE.V) {
    const _data = res.data['enclosureAnalysis']
    const beginMete = convertUnits(_data.surplusLength, 'mm', 'm', DP.MES_ENCLOSURE_L__M)
    const taskMete = convertUnits(_data.taskLength, 'mm', 'm', DP.MES_ENCLOSURE_L__M)
    const totalMete = convertUnits(_data.surplusLength + _data.taskLength, 'mm', 'm', DP.MES_ENCLOSURE_L__M)
    const completeMete = convertUnits(_data.completeLength, 'mm', 'm', DP.MES_ENCLOSURE_L__M)
    const inProductionMete = convertUnits(_data.inProductionLength, 'mm', 'm', DP.MES_ENCLOSURE_L__M)
    const unProducedMete = convertUnits(
      _data.surplusLength + _data.taskLength - _data.completeLength - _data.inProductionLength,
      'mm',
      'm',
      DP.MES_ENCLOSURE_L__M
    )
    // const totalQuantity = _data.surplusTaskQuantity + _data.taskQuantity
    const completeRate = Number(completeMete) ? toFixed((completeMete / totalMete) * 100, 2) + '%' : '0%'
    const inProductionRate = Number(inProductionMete) ? toFixed((inProductionMete / totalMete) * 100, 2) + '%' : '0%'
    // const unProducedQuantity = totalQuantity - _data.completeQuantity - _data.inProductionQuantity
    const unProducedRate = Number(unProducedMete) ? toFixed((unProducedMete / totalMete) * 100, 2) + '%' : '0%'
    res.data = [
      // {
      //   beginMete: _data.surplusTaskQuantity,
      //   taskMete: _data.taskQuantity,
      //   totalMete: totalQuantity,
      //   completeMete: _data.completeQuantity,
      //   inProductionMete: _data.inProductionQuantity,
      //   unProducedMete: unProducedQuantity
      // },
      { beginMete, taskMete, totalMete, completeRate, inProductionRate, completeMete, inProductionMete, unProducedMete, unProducedRate }
    ]
  }
}

const artifactDetailVisible = ref(false)
const enclosureDetailVisible = ref(false)

function toDetail() {
  if (crud.query.productType === reportComponentTypeEnum.ARTIFACT.V) {
    artifactDetailVisible.value = true
  } else {
    enclosureDetailVisible.value = true
  }
}
</script>

<style scoped>
::v-deep(.el-table__body-wrapper) {
  font-weight: 600;
}
</style>
