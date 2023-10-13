<template>
  <!-- 结构表格 -->
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
    <el-table-column prop="type" :show-overflow-tooltip="true" align="center" label="产品种类">
      <template v-slot="scope">
        <span :style="`color:${!isNotBlank(scope.row.originVal)?scope.row.color:''};`">
          <template v-if="isNotBlank(scope.row.originVal) && !judgeSameValue(scope.row.type,scope.row.originVal.type)">
            <cell-change-preview :old="scope.row.originVal.type" :new="scope.row.type" />
          </template>
          <template v-else>
            <span>{{ scope.row.type }}</span>
          </template>
        </span>
      </template>
    </el-table-column>
    <el-table-column prop="techDesc" :show-overflow-tooltip="true" align="center" label="技术要求描述">
      <template v-slot="scope">
        <span :style="`color:${!isNotBlank(scope.row.originVal)?scope.row.color:''};`">
          <template v-if="isNotBlank(scope.row.originVal) && !judgeSameValue(scope.row.techDesc,scope.row.originVal.techDesc)">
            <cell-change-preview :old="scope.row.originVal.techDesc" :new="scope.row.techDesc" />
          </template>
          <template v-else>
            <span>{{ scope.row.techDesc }}</span>
          </template>
        </span>
      </template>
    </el-table-column>
    <el-table-column prop="attachments" :show-overflow-tooltip="true" align="center" label="附件">
      <template v-slot="scope">
        <div v-if="isNotBlank(scope.row.originVal) && !judgeSameValue(scope.row.attachmentId,scope.row.originVal.attachmentId)">
          <cell-change-preview :old="scope.row.originVal.attachmentName" :new="scope.row.attachmentName" />
        </div>
        <div v-for="item in scope.row.attachments" :key="item.id" style="margin-bottom:2px;text-align:left;">
          <span @click="attachmentView(item)" :style="`color:${(item.name.indexOf('.pdf')>-1 || item.name.indexOf('.png')>-1 || item.name.indexOf('.jpg')>-1 || item.name.indexOf('.jpeg')>-1)?'#409eff':''};cursor:${(item.name.indexOf('.pdf')>-1 || item.name.indexOf('.png')>-1 || item.name.indexOf('.jpg')>-1 || item.name.indexOf('.jpeg')>-1)?'pointer':''}`">{{ item.name }}</span>
          <export-button :params="{id: item.id}" />
        </div>
      </template>
    </el-table-column>
  </common-table>
  <showPdfAndImg v-if="pdfShow" :isVisible="pdfShow" :showType="'attachment'" :id="currentId" @close="pdfShow=false"/>
</template>

<script setup>
import { ref, defineProps, computed } from 'vue'

import { judgeSameValue } from '@/views/contract/info/judgeSameValue'
import { isNotBlank } from '@data-type/index'

import ExportButton from '@comp-common/export-button/index.vue'
import showPdfAndImg from '@comp-base/show-pdf-and-img.vue'
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
    v.attachmentId = v.attachments?.map(k => k.id).join(',')
    v.attachmentName = v.attachments?.map(k => k.name).join(',')
    if (isNotBlank(props.originData)) {
      if (props.originData.findIndex(k => k.id === v.id) > -1) {
        const findVal = props.originData.find(k => k.id === v.id)
        findVal.attachmentId = findVal.attachments?.map(k => k.id).join(',')
        findVal.attachmentName = findVal.attachments?.map(k => k.name).join(',')
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

const pdfShow = ref(false)
const currentId = ref()

// 预览附件
function attachmentView(item) {
  const nameArr = item.name.split('.')
  const nameSuffix = nameArr[nameArr.length - 1]
  if (['jpg', 'png', 'pdf', 'jpeg'].indexOf(nameSuffix) > -1) {
    currentId.value = item.id
    pdfShow.value = true
  }
}

</script>
