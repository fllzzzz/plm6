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
        v-if="columns.visible('project.shortName')"
        key="project.shortName"
        prop="project.shortName"
        :show-overflow-tooltip="true"
        label="项目"
        min-width="250"
      >
        <template v-slot="scope">
          <span class="project-name">{{ projectNameFormatter(scope.row.project) }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('type')" key="type" prop="type" label="问题" align="center" width="100px">
        <template v-slot="scope">
          <span>{{ dict[scope.row.type] }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('applicantName')"
        key="applicantName"
        prop="applicantName"
        :show-overflow-tooltip="true"
        label="发起人"
        min-width="100px"
        align="center"
      />
      <el-table-column
        v-if="columns.visible('createTime')"
        key="createTime"
        prop="createTime"
        label="发起时间"
        align="center"
        width="140px"
      >
        <template v-slot="scope">
          <span v-parse-time="'{y}-{m}-{d} {h}:{i}'">{{ scope.row.createTime }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('description')"
        key="description"
        prop="description"
        :show-overflow-tooltip="true"
        label="描述"
        min-width="200px"
      />
      <el-table-column
        v-if="columns.visible('responsibleUserName')"
        key="responsibleUserName"
        prop="responsibleUserName"
        :show-overflow-tooltip="true"
        label="责任人"
        min-width="100px"
        align="center"
      />
      <el-table-column
        v-if="columns.visible('tinyImageUrl')"
        key="tinyImageUrl"
        prop="tinyImageUrl"
        :show-overflow-tooltip="false"
        label="问题图片"
        width="150px"
        align="left"
      >
        <template v-slot="scope">
          <div class="imgs-box">
            <el-image
              v-for="url in scope.row.tinyImageUrl"
              :preview-src-list="scope.row.imgUrls"
              :initial-index="1"
              :key="url"
              :src="url"
              lazy
            ></el-image>
          </div>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('status')"
        key="name"
        prop="name"
        :show-overflow-tooltip="true"
        label="状态"
        min-width="80px"
        align="center"
      >
        <template v-slot="scope">
          <el-tag v-if="improveStatusEnum.VK[scope.row.status]" :type="improveStatusEnum.V[scope.row.status].T">{{
            improveStatusEnum.VL[scope.row.status]
          }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('tinyImgs')"
        key="tinyImgs"
        prop="tinyImgs"
        :show-overflow-tooltip="false"
        label="整改图片"
        width="150px"
        align="left"
      >
        <template v-slot="scope">
          <div class="imgs-box">
            <el-image
              v-for="url in scope.row.tinyImgs"
              :preview-src-list="scope.row.imgs"
              :initial-index="1"
              :key="url"
              :src="url"
              lazy
            ></el-image>
          </div>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/QHSE-manage/disclosure'
import { ref } from 'vue'

import { improveStatusEnum } from '@enum-ms/mes'
import { projectNameFormatter } from '@/utils/project'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import useDict from '@compos/store/use-dict'
import pagination from '@crud/Pagination'
import mHeader from './module/header'

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

const dict = useDict(['breach_type'])
const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '问题曝光',
    invisibleColumns: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    return v
  })
}
</script>

<style lang="scss" scoped>
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
