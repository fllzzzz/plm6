<template>
  <!-- 压型楼层板表格 -->
  <common-table
    :data="tableArr"
    return-source-data
    :showEmptySymbol="false"
    border
  >
    <el-table-column label="序号" type="index" align="center" width="60" />
    <el-table-column prop="typeName" label="变更类型" type="index" align="center" width="60">
      <template v-slot="scope">
        <span :style="`color:${scope.row.color}`">{{scope.row.typeName}}</span>
      </template>
    </el-table-column>
    <el-table-column prop="plateType" :show-overflow-tooltip="true" align="center" label="板型">
      <template v-slot="scope">
        <span :style="`color:${!isNotBlank(scope.row.originVal)?scope.row.color:''};`">
          <template v-if="isNotBlank(scope.row.originVal) && !judgeSameValue(scope.row.plateType,scope.row.originVal.plateType)">
            <cell-change-preview :old="scope.row.originVal.plateType" :new="scope.row.plateType" />
          </template>
          <template v-else>
            <span>{{ scope.row.plateType }}</span>
          </template>
        </span>
      </template>
    </el-table-column>
     <el-table-column prop="unfoldedWidth" :show-overflow-tooltip="true" align="center" label="展宽(mm)">
      <template v-slot="scope">
        <span :style="`color:${!isNotBlank(scope.row.originVal)?scope.row.color:''};`">
          <template v-if="isNotBlank(scope.row.originVal) && !judgeSameValue(scope.row.unfoldedWidth,scope.row.originVal.unfoldedWidth)">
            <cell-change-preview :old="scope.row.originVal.unfoldedWidth" :new="scope.row.unfoldedWidth" />
          </template>
          <template v-else>
            <span>{{ scope.row.unfoldedWidth }}</span>
          </template>
        </span>
      </template>
    </el-table-column>
    <el-table-column prop="effectiveWidth" :show-overflow-tooltip="true" align="center" label="有效宽(mm)">
      <template v-slot="scope">
        <span :style="`color:${!isNotBlank(scope.row.originVal)?scope.row.color:''};`">
          <template v-if="isNotBlank(scope.row.originVal) && !judgeSameValue(scope.row.effectiveWidth,scope.row.originVal.effectiveWidth)">
            <cell-change-preview :old="scope.row.originVal.effectiveWidth" :new="scope.row.effectiveWidth" />
          </template>
          <template v-else>
            <span>{{ scope.row.effectiveWidth }}</span>
          </template>
        </span>
      </template>
    </el-table-column>
    <el-table-column prop="mode" :show-overflow-tooltip="true" align="center" label="类型">
      <template v-slot="scope">
        <span :style="`color:${!isNotBlank(scope.row.originVal)?scope.row.color:''};`">
          <template v-if="isNotBlank(scope.row.originVal) && !judgeSameValue(scope.row.mode,scope.row.originVal.mode)">
            <cell-change-preview :old="scope.row.originVal.mode" :new="scope.row.mode" />
          </template>
          <template v-else>
            <span>{{ scope.row.mode }}</span>
          </template>
        </span>
      </template>
    </el-table-column>
    <el-table-column prop="thickness" :show-overflow-tooltip="true" align="center" label="板厚(mm)">
      <template v-slot="scope">
        <span :style="`color:${!isNotBlank(scope.row.originVal)?scope.row.color:''};`">
          <template v-if="isNotBlank(scope.row.originVal) && !judgeSameValue(scope.row.thickness,scope.row.originVal.thickness)">
            <cell-change-preview :old="scope.row.originVal.thickness" :new="scope.row.thickness" />
          </template>
          <template v-else>
            <span>{{ scope.row.thickness }}</span>
          </template>
        </span>
      </template>
    </el-table-column>
    <el-table-column prop="yieldStrength" :show-overflow-tooltip="true" align="center" label="屈服强度">
      <template v-slot="scope">
        <span :style="`color:${!isNotBlank(scope.row.originVal)?scope.row.color:''};`">
          <template v-if="isNotBlank(scope.row.originVal) && !judgeSameValue(scope.row.yieldStrength,scope.row.originVal.yieldStrength)">
            <cell-change-preview :old="scope.row.originVal.yieldStrength" :new="scope.row.yieldStrength" />
          </template>
          <template v-else>
            <span>{{ scope.row.yieldStrength }}</span>
          </template>
        </span>
      </template>
    </el-table-column>
    <el-table-column prop="brand" :show-overflow-tooltip="true" align="center" label="品牌">
      <template v-slot="scope">
         <span :style="`color:${!isNotBlank(scope.row.originVal)?scope.row.color:''};`">
          <template v-if="isNotBlank(scope.row.originVal) && !judgeSameValue(scope.row.brand,scope.row.originVal.brand)">
            <cell-change-preview :old="scope.row.originVal.brand" :new="scope.row.brand" />
          </template>
          <template v-else>
            <span>{{ scope.row.brand }}</span>
          </template>
        </span>
      </template>
    </el-table-column>
    <el-table-column prop="plating" :show-overflow-tooltip="true" align="center" label="镀层(g)">
      <template v-slot="scope">
        <span :style="`color:${!isNotBlank(scope.row.originVal)?scope.row.color:''};`">
          <template v-if="isNotBlank(scope.row.originVal) && !judgeSameValue(scope.row.plating,scope.row.originVal.plating)">
            <cell-change-preview :old="scope.row.originVal.plating" :new="scope.row.plating" />
          </template>
          <template v-else>
            <span>{{ scope.row.plating }}</span>
          </template>
        </span>
      </template>
    </el-table-column>
    <el-table-column prop="quantity" :show-overflow-tooltip="true" align="center" label="数量(m)">
      <template v-slot="scope">
         <span :style="`color:${!isNotBlank(scope.row.originVal)?scope.row.color:''};`">
          <template v-if="isNotBlank(scope.row.originVal) && !judgeSameValue(scope.row.quantity,scope.row.originVal.quantity)">
            <cell-change-preview :old="scope.row.originVal.quantity" :new="scope.row.quantity" />
          </template>
          <template v-else>
            <span>{{ scope.row.quantity }}</span>
          </template>
        </span>
      </template>
    </el-table-column>
  </common-table>
</template>

<script setup>
import { defineProps, computed } from 'vue'
import { judgeSameValue } from '@/views/contract/info/judgeSameValue'
import { isNotBlank } from '@data-type/index'

import cellChangePreview from '@comp-common/cell-change-preview'

const props = defineProps({
  tableData: {
    type: Array,
    default: () => []
  },
  originData: {
    type: Array,
    default: () => []
  }
})

const tableArr = computed(() => {
  const arr = []
  props.tableData?.forEach(v => {
    if (isNotBlank(props.originData)) {
      if (props.originData.findIndex(k => k.id === v.id) > -1) {
        const findVal = props.originData.find(k => k.id === v.id)
        if (judgeSameValue(v, findVal)) {
          arr.push({
            ...v,
            typeName: '无变更',
            color: '#909399'
          })
        } else {
          arr.push({
            ...v,
            originVal: findVal,
            typeName: '修改',
            color: '#e6a23c'
          })
        }
      } else {
        arr.push({
          ...v,
          typeName: '新增',
          color: 'green'
        })
      }
    } else {
      arr.push({
        ...v,
        typeName: '新增',
        color: 'green'
      })
    }
  })
  props.originData.forEach(v => {
    if (arr.findIndex(k => k.id === v.id) < 0) {
      arr.push({
        ...v,
        typeName: '删除',
        color: 'red'
      })
    }
  })
  return arr
})
</script>
