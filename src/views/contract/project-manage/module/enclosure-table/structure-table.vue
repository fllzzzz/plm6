<template>
  <!-- 结构表格 -->
  <common-table
    :data="tableData"
    return-source-data
    :showEmptySymbol="false"
    border
  >
    <el-table-column label="序号" type="index" align="center" width="60" />
    <el-table-column prop="type" :show-overflow-tooltip="true" align="center" label="产品种类">
      <template v-slot="scope">
        <span>{{ scope.row.type }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="techDesc" :show-overflow-tooltip="true" align="center" label="技术要求描述">
      <template v-slot="scope">
        <span>{{ scope.row.techDesc }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="attachments" :show-overflow-tooltip="true" align="center" label="附件">
      <template v-slot="scope">
        <div v-for="item in scope.row.attachments" :key="item.id" style="margin-bottom:2px;text-align:left;">
          <span @click="attachmentView(item)" :style="`color:${(item.name.indexOf('.pdf')>-1 || item.name.indexOf('.png')>-1 || item.name.indexOf('.jpg')>-1 || item.name.indexOf('.jpeg')>-1)?'#409eff':''};cursor:${(item.name.indexOf('.pdf')>-1 || item.name.indexOf('.png')>-1 || item.name.indexOf('.jpg')>-1 || item.name.indexOf('.jpeg')>-1)?'pointer':''}`">{{ item.name }}</span>
          <export-button :params="{id: item.id}" />
        </div>
      </template>
    </el-table-column>
    <el-table-column v-if="!isShow" label="操作" align="center">
      <template v-slot="scope">
        <common-button size="small" class="el-icon-edit" type="primary" @click="editRow(scope.$index,scope.row)" />
        <common-button size="small" class="el-icon-delete" type="danger" @click="deleteRow(scope.$index)" />
      </template>
    </el-table-column>
  </common-table>
  <showPdfAndImg v-if="pdfShow" :isVisible="pdfShow" :showType="'attachment'" :id="currentId" @close="pdfShow=false"/>
</template>

<script setup>
import { ref, defineProps, defineEmits, watch } from 'vue'

import ExportButton from '@comp-common/export-button/index.vue'
import showPdfAndImg from '@comp-base/show-pdf-and-img.vue'

const props = defineProps({
  tableData: {
    type: Array,
    default: () => []
  },
  isShow: {
    type: Boolean,
    default: true
  }
})

const techTableData = ref([])

watch(
  () => props.tableData,
  (val) => {
    techTableData.value = props.tableData
  },
  { deep: true, immediate: true }
)

const emit = defineEmits(['edit'])

const pdfShow = ref(false)
const currentId = ref()

function deleteRow(index) {
  techTableData.value.splice(index, 1)
  // props.tableData.splice(index, 1)
}

// 预览附件
function attachmentView(item) {
  const nameArr = item.name.split('.')
  const nameSuffix = nameArr[nameArr.length - 1]
  if (['jpg', 'png', 'pdf', 'jpeg'].indexOf(nameSuffix) > -1) {
    currentId.value = item.id
    pdfShow.value = true
  }
}

function editRow(index, row) {
  emit('edit', row)
  techTableData.value.splice(index, 1)
  // props.tableData.splice(index, 1)
}
</script>
