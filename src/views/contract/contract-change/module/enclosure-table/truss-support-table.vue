<template>
  <!-- 桁架楼层板表格 -->
  <common-table
    :data="tableArr"
    return-source-data
    :showEmptySymbol="false"
    border
  >
    <el-table-column :label="'序号'" type="index" align="center" width="60" />
    <el-table-column prop="typeName" label="变更类型" type="index" align="center" width="60">
      <template v-slot="scope">
        <span :style="`color:${scope.row.color}`">{{scope.row.typeName}}</span>
      </template>
    </el-table-column>
    <el-table-column prop="serialNumber" :show-overflow-tooltip="true" align="center" label="板型">
      <template v-slot="scope">
        <span :style="`color:${!isNotBlank(scope.row.originVal)?scope.row.color:''};`">
          <template v-if="isNotBlank(scope.row.originVal) && !judgeSameValue(scope.row.serialNumber,scope.row.originVal.serialNumber)">
            <cell-change-preview :old="scope.row.originVal.serialNumber" :new="scope.row.serialNumber" />
          </template>
          <template v-else>
            <span>{{ scope.row.serialNumber }}</span>
          </template>
        </span>
      </template>
    </el-table-column>
    <el-table-column prop="basementMembrane" :show-overflow-tooltip="true" align="center" label="底膜(mm)">
      <template v-slot="scope">
        <span :style="`color:${!isNotBlank(scope.row.originVal)?scope.row.color:''};`">
          <template v-if="isNotBlank(scope.row.originVal) && !judgeSameValue(scope.row.basementMembrane,scope.row.originVal.basementMembrane)">
            <cell-change-preview :old="scope.row.originVal.basementMembrane" :new="scope.row.basementMembrane" />
          </template>
          <template v-else>
            <span>{{ scope.row.basementMembrane }}</span>
          </template>
        </span>
      </template>
    </el-table-column>
    <el-table-column prop="effectiveWidth" :show-overflow-tooltip="true" align="center" label="有效宽度">
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
    <el-table-column prop="firstQuarter" :show-overflow-tooltip="true" align="center" label="上弦筋(φ)">
      <template v-slot="scope">
        <span :style="`color:${!isNotBlank(scope.row.originVal)?scope.row.color:''};`">
          <template v-if="isNotBlank(scope.row.originVal) && !judgeSameValue(scope.row.firstQuarter,scope.row.originVal.firstQuarter)">
            <cell-change-preview :old="scope.row.originVal.firstQuarter" :new="scope.row.firstQuarter" />
          </template>
          <template v-else>
            <span>{{ scope.row.firstQuarter }}</span>
          </template>
        </span>
      </template>
    </el-table-column>
    <el-table-column prop="lastQuarter" :show-overflow-tooltip="true" align="center" label="下弦筋(φ)">
      <template v-slot="scope">
        <span :style="`color:${!isNotBlank(scope.row.originVal)?scope.row.color:''};`">
          <template v-if="isNotBlank(scope.row.originVal) && !judgeSameValue(scope.row.lastQuarter,scope.row.originVal.lastQuarter)">
            <cell-change-preview :old="scope.row.originVal.lastQuarter" :new="scope.row.lastQuarter" />
          </template>
          <template v-else>
            <span>{{ scope.row.lastQuarter }}</span>
          </template>
        </span>
      </template>
    </el-table-column>
    <el-table-column prop="webMember" :show-overflow-tooltip="true" align="center" label="腹杆筋(φ)">
      <template v-slot="scope">
        <span :style="`color:${!isNotBlank(scope.row.originVal)?scope.row.color:''};`">
          <template v-if="isNotBlank(scope.row.originVal) && !judgeSameValue(scope.row.webMember,scope.row.originVal.webMember)">
            <cell-change-preview :old="scope.row.originVal.webMember" :new="scope.row.webMember" />
          </template>
          <template v-else>
            <span>{{ scope.row.webMember }}</span>
          </template>
        </span>
      </template>
    </el-table-column>
    <el-table-column prop="vertical" :show-overflow-tooltip="true" align="center" label="竖向筋(φ)">
      <template v-slot="scope">
        <span :style="`color:${!isNotBlank(scope.row.originVal)?scope.row.color:''};`">
          <template v-if="isNotBlank(scope.row.originVal) && !judgeSameValue(scope.row.vertical,scope.row.originVal.vertical)">
            <cell-change-preview :old="scope.row.originVal.vertical" :new="scope.row.vertical" />
          </template>
          <template v-else>
            <span>{{ scope.row.vertical }}</span>
          </template>
        </span>
      </template>
    </el-table-column>
    <el-table-column prop="level" :show-overflow-tooltip="true" align="center" label="水平筋(φ)">
      <template v-slot="scope">
        <span :style="`color:${!isNotBlank(scope.row.originVal)?scope.row.color:''};`">
          <template v-if="isNotBlank(scope.row.originVal) && !judgeSameValue(scope.row.level,scope.row.originVal.level)">
            <cell-change-preview :old="scope.row.originVal.level" :new="scope.row.level" />
          </template>
          <template v-else>
            <span>{{ scope.row.level }}</span>
          </template>
        </span>
      </template>
    </el-table-column>
    <el-table-column prop="weightMeter" :show-overflow-tooltip="true" align="center" label="米重(kg/m)">
      <template v-slot="scope">
        <span :style="`color:${!isNotBlank(scope.row.originVal)?scope.row.color:''};`">
          <template v-if="isNotBlank(scope.row.originVal) && !judgeSameValue(scope.row.weightMeter,scope.row.originVal.weightMeter)">
            <cell-change-preview :old="scope.row.originVal.weightMeter" :new="scope.row.weightMeter" />
          </template>
          <template v-else>
            <span>{{ scope.row.weightMeter }}</span>
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
