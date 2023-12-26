<template>
  <div class="app-container">
    <mHeader :showType="props.showType" />
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :data-format="dataFormat"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      style="width: 100%;margin-top:10px;"
      class="collection-table"
      :stripe="false"
    >
      <el-table-column prop="index" label="序号" align="center" width="50" type="index" />
      <el-table-column key="receiptSerialNumber" prop="receiptSerialNumber" v-if="columns.visible('receiptSerialNumber')" label="转换单号" align="center" min-width="150" show-overflow-tooltip />
      <el-table-column key="outSerialNumber" prop="outSerialNumber" v-if="columns.visible('outSerialNumber') && showType !== 'coilPlate'" label="出库单号" align="center" show-overflow-tooltip />
      <el-table-column key="serialNumber" prop="serialNumber" v-if="columns.visible('serialNumber')" label="物料编号" align="center" show-overflow-tooltip width="100"/>
      <el-table-column key="classifyName" prop="classifyName" v-if="columns.visible('classifyName')" label="物料名称" align="center" show-overflow-tooltip width="100" />
      <el-table-column key="specification" prop="specification" v-if="columns.visible('specification')" label="规格" align="center" show-overflow-tooltip/>
      <el-table-column key="widthThick" prop="widthThick" v-if="columns.visible('widthThick')" label="厚(mm)*宽(mm)" align="center" show-overflow-tooltip min-width="120" />
      <el-table-column key="brand" prop="brand" v-if="columns.visible('brand')" label="品牌" align="center" show-overflow-tooltip/>
      <el-table-column key="color" prop="color" v-if="columns.visible('color')" label="颜色" align="center" show-overflow-tooltip/>
      <el-table-column key="heatNoAndBatchNo" prop="heatNoAndBatchNo" v-if="columns.visible('heatNoAndBatchNo')" label="卷号" align="center" show-overflow-tooltip/>
      <el-table-column key="project" prop="project" v-if="columns.visible('project')" label="所属项目" align="center" show-overflow-tooltip/>
      <el-table-column key="warehouse.name" prop="warehouse.name" v-if="columns.visible('warehouse.name')" label="仓库位置" align="center" show-overflow-tooltip/>
      <el-table-column key="applyTime" prop="applyTime" v-if="columns.visible('applyTime')" label="申请时间" align="center" show-overflow-tooltip/>
      <el-table-column key="applyUserName" prop="applyUserName" v-if="columns.visible('applyUserName')" label="申请人" align="center" show-overflow-tooltip/>
      <el-table-column key="reviewTime" prop="reviewTime" v-if="columns.visible('reviewTime') && showType !== 'coilPlate'" label="审核时间" align="center" show-overflow-tooltip/>
      <el-table-column key="reviewerName" prop="reviewerName" v-if="columns.visible('reviewerName') && showType !== 'coilPlate'" label="审核人" align="center" show-overflow-tooltip/>
      <el-table-column
        v-if="columns.visible('status')"
        key="status"
        :show-overflow-tooltip="true"
        prop="status"
        label="审核状态"
        align="center"
        width="80"
      >
        <template #default="{ row: { sourceRow: row } }">
          <template v-if="checkPermission(permission.audit) && row.status===reviewStatusEnum.UNREVIEWED.V">
            <common-button type="warning" icon="el-icon-s-check" size="mini" @click="toReview(row)" />
          </template>
          <template v-else>
            <el-tag :type="reviewStatusEnum.V[row.status]?.TAG">{{ reviewStatusEnum.VL[row.status] }}</el-tag>
          </template>
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column
        v-if="checkPermission([...permission.detail])"
        label="操作"
        width="80px"
        align="center"
      >
        <template #default="{ row }">
          <udOperation :data="row" show-detail :show-edit="false" :show-del="false" />
          <!-- <common-button icon="el-icon-view" type="primary" size="mini" @click="openDetail(scope.row)" v-permission="permission.detail"/> -->
        </template>
      </el-table-column>
    </common-table>
    <mDetail />
    <!-- 审核 -->
    <review v-model="reviewVisible" :detail-info="currentRow" @refresh="handleListRefresh" />
  <!--分页组件-->
  <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/wms/report/raw-material/convert-list'
import { ref, computed, defineEmits, defineProps, watch } from 'vue'

import checkPermission from '@/utils/system/check-permission'
import { rawMaterialConvertListPM as permission } from '@/page-permission/wms'
import { reviewStatusEnum } from '@enum-ms/common'

import UdOperation from '@crud/UD.operation.vue'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import mDetail from './module/detail'
import Review from './module/review.vue'

const emit = defineEmits(['success'])

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const props = defineProps({
  showType: {
    type: String,
    default: undefined
  },
  visible: {
    type: Boolean,
    default: undefined
  }
})

const dataFormat = computed(() => {
  return [
    ['applyTime', ['parse-time', '{y}-{m}-{d}']],
    ['reviewTime', ['parse-time', '{y}-{m}-{d}']],
    ['project', 'parse-project']
  ]
})

const tableRef = ref()
const reviewVisible = ref(false)
const currentRow = ref({})

const { crud, CRUD, columns } = useCRUD(
  {
    title: '存货转换单',
    sort: [],
    permission: { ...permission },
    invisibleColumns: ['applyTime', 'applyUserName', 'reviewTime', 'reviewerName'],
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

watch(
  () => props.visible,
  (visible) => {
    if (visible) {
      crud.toQuery()
    }
  },
  { immediate: true }
)

const { maxHeight } = useMaxHeight({
  paginate: true,
  extraHeight: 40
})

// 打开审核
function toReview(row) {
  currentRow.value = row
  reviewVisible.value = true
}

function handleListRefresh() {
  emit('success')
  crud.refresh()
}

CRUD.HOOK.handleRefresh = (crud, { data }) => {
  data.content.forEach(v => {
    v.widthThick = (v.thickness || v.width) ? (v.thickness || '-') + '*' + (v.width || '-') : '-'
  })
}
</script>

<style lang="scss" scoped>
.collection-table{
  ::v-deep(.el-select .el-input__inner){
    padding-left:2px;
    padding-right:5px;
  }
  ::v-deep(.el-input-number .el-input__inner, .el-input__inner) {
    text-align: left;
    padding:0 5px;
  }
  ::v-deep(.el-table .cell){
    padding-left:2px;
    padding-right:2px;
  }
}
.imgs-box {
  & > .el-image {
    width: 50px;
    height: 40px;
    border: 2px solid #dcdfe6;
    border-radius: 6px;
    background-color: white;
    cursor: pointer;
    + .el-image {
      margin-left: -40px;
    }
  }
}
</style>
