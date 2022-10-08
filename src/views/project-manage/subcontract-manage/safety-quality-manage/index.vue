<template>
  <div class="app-container">
    <mHeader />
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
      :showEmptySymbol="false"
    >
      <el-table-column prop="index" label="序号" align="center" width="50" type="index" />
      <el-table-column key="project" prop="project" label="所属项目" align="center" min-width="150" show-overflow-tooltip />
      <el-table-column key="supplierName" prop="supplierName" label="分包单位" align="center" show-overflow-tooltip />
      <el-table-column key="subcontractClassName" prop="subcontractClassName" label="分类" align="center" show-overflow-tooltip />
      <el-table-column key="problemDate" prop="problemDate" label="日期" align="center" show-overflow-tooltip width="100"/>
      <el-table-column key="promoterName" prop="promoterName" label="发起人" align="center" show-overflow-tooltip width="100" />
      <el-table-column key="problemTypeName" prop="problemTypeName" label="问题分类" align="center" show-overflow-tooltip/>
      <el-table-column
        v-if="columns.visible('beforeChangeImgs')"
        key="beforeChangeImgs"
        prop="beforeChangeImgs"
        :show-overflow-tooltip="false"
        label="整改前"
        width="180px"
        align="left"
      >
        <template #default="{ row }">
          <div class="imgs-box">
            <el-image
              v-for="url in row.sourceRow.beforeChangeImgs"
              :preview-src-list="row.beforeImgSrc"
              :initial-index="1"
              :key="url.id"
              :src="url.tinyImageUrl"
              lazy
              style="margin:0 2px;"
            ></el-image>
          </div>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('afterChangeImgs')"
        key="afterChangeImgs"
        prop="afterChangeImgs"
        :show-overflow-tooltip="false"
        label="整改后"
        width="180px"
        align="left"
      >
        <template #default="{ row }">
          <div class="imgs-box">
            <el-image
              v-for="url in row.sourceRow.afterChangeImgs"
              :preview-src-list="row.afterImgSrc"
              :initial-index="1"
              :key="url.id"
              :src="url.tinyImageUrl"
              style="margin:0 2px;"
              lazy
            ></el-image>
          </div>
        </template>
      </el-table-column>
      <el-table-column key="penalty" prop="penalty" label="违约金" align="center" show-overflow-tooltip />
      <el-table-column key="boolChangeStatus" prop="boolChangeStatus" label="状态" align="center">
        <template v-slot="scope">
          <el-tag :type="scope.row.boolChangeStatus?'success':'warning'" >{{ qualityProblemChangeType.VL[scope.row.boolChangeStatus] }}</el-tag>
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column
        v-if="checkPermission([...permission.detail,...permission.audit,...permission.close])"
        label="操作"
        width="190px"
        align="center"
      >
        <template v-slot="scope">
          <common-button icon="el-icon-view" type="primary" size="mini" @click="openDetail(scope.row,'detail')" v-permission="permission.detail"/>
          <common-button size="mini" icon="el-icon-s-check" type="primary" v-if="scope.row.qhseChange?.auditStatus===auditTypeEnum.AUDITING.V && checkPermission(permission.audit) && scope.row.boolReply" @click="openDetail(scope.row,'audit')"/>
          <common-button size="mini" type="primary" v-if="scope.row.qhseChange?.auditStatus===auditTypeEnum.AUDITING.V && checkPermission(permission.close) && !scope.row.boolReply" @click="openDetail(scope.row,'close')">关闭</common-button>
        </template>
      </el-table-column>
    </common-table>
    <mDetail :detail-info="currentRow" v-model="detailVisible" :showType="showType" @success="crud.toQuery" :permission="permission" />
  <!--分页组件-->
  <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/project-manage/quality-problem-manage'
import { ref } from 'vue'

import checkPermission from '@/utils/system/check-permission'
import { auditTypeEnum } from '@enum-ms/contract'
import { qualityProblemChangeType } from '@enum-ms/project'
import { qualitySafetyPM as permission } from '@/page-permission/project'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import mDetail from './module/detail'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const dataFormat = ref([
  ['problemDate', ['parse-time', '{y}-{m}-{d}']],
  ['project', 'parse-project'],
  ['penalty', 'to-thousand']
])

const tableRef = ref()
const detailVisible = ref(false)
const currentRow = ref({})
const showType = ref('detail')

const { crud, CRUD, columns } = useCRUD(
  {
    title: '质安管理',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  paginate: true,
  extraHeight: 40
})

function openDetail(row, type) {
  currentRow.value = row.sourceRow
  currentRow.value.beforeImgSrc = row.sourceRow.beforeImgSrc
  currentRow.value.afterImgSrc = row.sourceRow.afterImgSrc
  currentRow.value.auditStatus = row.sourceRow.qhseChange?.auditStatus
  currentRow.value.problemDesc = row.sourceRow.qhseChange?.problemDesc
  currentRow.value.afterChangeImgs = row.sourceRow.qhseChange?.afterChangeImgs
  showType.value = type
  detailVisible.value = true
}

CRUD.HOOK.handleRefresh = (crud, { data }) => {
  data.content.forEach(v => {
    v.beforeImgSrc = v.beforeChangeImgs && v.beforeChangeImgs.map((k) => k.imageUrl)
    v.afterImgSrc = v.afterChangeImgs && v.afterChangeImgs.map((k) => k.imageUrl)
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
