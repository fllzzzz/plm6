<template>
  <div class="head-container">
    <div v-if="crud.searchToggle">
      <common-radio-button
        v-model="query.materialLabelPrintType"
        :options="materialLabelPrintTypeEnum.ENUM"
        show-option-all
        type="enum"
        size="small"
        class="filter-item"
        @change="handleBasicClassChange"
      />
      <common-radio-button
        v-model="query.basicClass"
        :options="steelClsEnum.ENUM"
        show-option-all
        :option-all-value="STEEL_ENUM"
        type="enum"
        size="small"
        class="filter-item"
        @change="handleBasicClassChange"
      />
      <common-radio-button
        v-model="query.boolPrinted"
        :options="boolPrintedEnum.ENUM"
        show-option-all
        type="enum"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      />
      <el-date-picker
        v-model="query.createDate"
        type="daterange"
        range-separator=":"
        size="small"
        class="filter-item"
        start-placeholder="开始日期"
        end-placeholder="结束日期"
        style="width: 300px"
        clearable
        :shortcuts="PICKER_OPTIONS_SHORTCUTS"
        value-format="x"
        @change="crud.toQuery"
      />
      <br/>
      <warehouse-project-cascader
        v-model:projectId="query.projectId"
        v-model:projectWarehouseType="query.projectWarehouseType"
        class="filter-item"
        @change="crud.toQuery"
      />
    </div>
    <crudOperation>
      <!-- TODO:打印 -->
      <template #optLeft>
        <common-button class="filter-item" type="success" size="mini" icon="el-icon-printer" @click="toBatchPrint">批量打印</common-button>
        <el-select v-model="crud.props.copies" placeholder="可选择打印份数" size="mini" style="width: 100px">
          <el-option v-for="i in 3" :key="`copies_${i}`" :label="`打印${i}份`" :value="i" />
        </el-select>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { defineEmits } from 'vue'
import { STEEL_ENUM } from '@/settings/config'
import { steelClsEnum } from '@/utils/enum/modules/classification'
import { boolPrintedEnum } from '@/utils/enum/modules/common'
import { materialLabelPrintTypeEnum } from '@/utils/enum/modules/wms'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'

import usePrint from '../../composables/use-print'
import useGetPrintInfo from '../../composables/use-get-print-info'
import warehouseProjectCascader from '@comp-wms/warehouse-project-cascader'
import { ElMessage } from 'element-plus'

const emit = defineEmits(['printed-success'])

const defaultQuery = {
  boolPrinted: false, // 打印状态
  materialLabelPrintType: undefined, // 物料标签打印类型
  projectWarehouseType: undefined, // 仓库类型
  projectId: undefined, // 项目id
  basicClass: STEEL_ENUM, // 基础类型
  createDate: [] // 创建日期
}

const { crud, query } = regHeader(defaultQuery)
const { print } = usePrint({ emit })
const { getDetailMaterialList } = useGetPrintInfo()

// 基础类型发生变化
async function handleBasicClassChange(val) {
  crud.data = []
  crud.setColumns()
  crud.toQuery()
}

// 批量打印
async function toBatchPrint() {
  if (!crud.selections || crud.selections.length === 0) {
    ElMessage.warning('请选择需要打印的单据')
    return
  }
  const ids = crud.selections.map((row) => row.id)
  await print(() => getDetailMaterialList(ids), crud.props.copies)
  crud.refresh()
}
</script>
