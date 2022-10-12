<template>
  <div class="detail-container">
    <!--工具栏-->
    <mHeader class="head-container" :tip="props.tip" />
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      :data-format="columnsDataFormat"
      style="width: 100%"
      @selection-change="crud.selectionChangeHandler"
    >
      <el-table-column type="selection" :selectable="selectable" width="55" align="center" />
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column v-if="columns.visible('areaName')" key="areaName" prop="areaName" show-overflow-tooltip label="区域" align="center"/>
      <el-table-column v-if="columns.visible('serialNumber')" key="serialNumber" prop="serialNumber" show-overflow-tooltip label="编号" align="center"/>
      <el-table-column v-if="columns.visible('specification')" key="specification" prop="specification" show-overflow-tooltip label="规格" align="center"/>
      <el-table-column v-if="columns.visible('quantity')" key="quantity" prop="quantity" show-overflow-tooltip label="清单数量" align="center"/>
      <el-table-column v-if="columns.visible('createUser')" key="createUser" prop="createUser" show-overflow-tooltip label="创建者" align="center"/>
      <el-table-column v-if="columns.visible('createTime')" key="createTime" prop="createTime" show-overflow-tooltip label="创建时间" align="center"/>
      <el-table-column v-if="columns.visible('selectable')" key="selectable" prop="selectable" show-overflow-tooltip label="匹配状态" align="center" width="100">
        <template v-slot="scope">
          <el-tag v-if="scope.row.selectable" type="success" size="medium">匹配成功</el-tag>
          <el-tag v-else type="danger" size="medium">未上传</el-tag>
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column
        label="操作"
        width="170px"
        align="center"
        fixed="right"
      >
        <template v-slot="scope">
          <div class="btn">
            <upload-btn
              v-permission="permission.import"
              :dataType="scope.row.dataType"
              :accept="props.uploadType"
              :data="getParams(scope.row)"
              :upload-fun="update"
              :tip="props.uploadType"
              btn-name="覆盖导入"
              :disabled="!scope.row.selectable"
              size="mini"
              type="primary"
              @success="handleSuccess"
            />
            <udOperation :show-edit="false" :data="scope.row" :disabled-del="!scope.row.selectable" :permission="permission" />
          </div>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import { detail, del, update } from '@/api/plan/technical-data-manage/technical-achievement'
import { ref, watch, defineProps, defineEmits } from 'vue'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import udOperation from '@crud/UD.operation'
import uploadBtn from '@/views/plan/technical-data-manage/technical-achievement/components/drawing-upload-btn.vue'
import mHeader from './module/header'

const emit = defineEmits(['success'])

const props = defineProps({
  currentRow: {
    type: Object,
    default: () => {}
  },
  permission: {
    type: Object,
    default: () => {}
  },
  uploadType: {
    type: String,
    default: '.pdf'
  },
  tip: {
    type: String,
    default: '覆盖导入仅支持PDF格式'
  }
})

const optShow = {
  add: false,
  edit: false,
  del: true,
  download: false
}

const tableRef = ref()
// 表格列数据格式转换
const columnsDataFormat = ref([
  ['createTime', 'parse-time']
])

const { CRUD, crud, columns } = useCRUD(
  {
    title: 'XML文件匹配详情',
    permission: { ...props.permission },
    optShow: { ...optShow },
    crudApi: { get: detail, del },
    requiredQuery: ['monomerId'],
    invisibleColumns: [],
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.detail-container',
  extraBox: '.detail-container .head-container',
  navbar: false,
  paginate: true,
  extraHeight: 190
})

watch(
  () => props.currentRow,
  (row = {}) => {
    crud.query.projectId = row.projectId
    crud.query.monomerId = row.monomerId
    crud.query.dataType = row.dataType
    crud.query.productType = row.productType
    crud.toQuery()
  },
  { immediate: true, deep: true }
)

// 处理数据
CRUD.HOOK.handleRefresh = (crud, { data: { content = [] }}) => {
  content.forEach((v) => {
    v.id = {
      ids: v.drawingID || []
    }
    v.selectable = !!v.sign
  })
}

// 处理数据
CRUD.HOOK.afterDelete = () => {
  handleSuccess()
}

// 获取参数
function getParams(row) {
  return {
    productId: row.productId,
    projectId: row.projectId,
    monomerId: row.monomerId,
    productType: row.productType,
    dataType: row.dataType
  }
}

// 上传成功
function handleSuccess() {
  crud.toQuery()
  emit('success')
}

// 是否上传
function selectable(row) {
  return row.selectable
}
</script>

<style lang="scss" scoped>
::v-deep(.el-table) {
  .btn {
    *:nth-child(2){
      margin-left: 6px;
    }
  }
}
</style>
