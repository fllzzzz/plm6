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
        v-if="columns.visible('type')"
        :show-overflow-tooltip="true"
        prop="type"
        label="问题类型"
        align="center"
        width="100px"
      >
        <template v-slot="scope">
          <span>{{ problemTypeEnum.VL[scope.row.type] }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('userName')"
        :show-overflow-tooltip="true"
        prop="userName"
        label="发起人"
        align="center"
        width="140px"
      >
        <template v-slot="scope">
          <span style="white-space: pre-line">
            <span>{{ scope.row.userName + '\n' }}</span>
            <span v-parse-time="'{y}-{m}-{d} {h}:{i}'">{{ scope.row.createTime }}</span>
          </span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('projectName')" prop="projectName" :show-overflow-tooltip="true" label="项目" min-width="250">
        <template v-slot="scope">
          <span>{{ scope.row.projectName }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('qualityTypeListStr')"
        :show-overflow-tooltip="true"
        prop="qualityTypeListStr"
        label="问题"
        min-width="100px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.qualityTypeListStr }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('remark')"
        key="remark"
        prop="remark"
        :show-overflow-tooltip="true"
        label="描述"
        min-width="200px"
      />
      <el-table-column
        v-if="columns.visible('dutyName')"
        key="dutyName"
        prop="dutyName"
        :show-overflow-tooltip="true"
        label="责任人"
        min-width="100px"
        align="center"
      />
      <el-table-column
        v-if="columns.visible('attachmentDTOS')"
        key="attachmentDTOS"
        prop="attachmentDTOS"
        :show-overflow-tooltip="false"
        label="问题图片"
        width="150px"
        align="left"
      >
        <template v-slot="scope">
          <div class="imgs-box">
            <el-image
              v-for="url in scope.row.attachmentDTOS"
              :preview-src-list="scope.row.imgUrls"
              :initial-index="1"
              :key="url.id"
              :src="url.tinyImageUrl"
              lazy
            ></el-image>
          </div>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('status')"
        key="status"
        prop="status"
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
      <el-table-column
        v-if="columns.visible('rectifyName')"
        :show-overflow-tooltip="true"
        prop="rectifyName"
        label="整改人"
        align="center"
        width="140px"
      >
        <template v-slot="scope">
          <span style="white-space: pre-line">
            <span v-empty-text>{{ scope.row.rectifyName }}</span>{{'\n'}}
            <span v-parse-time="'{y}-{m}-{d} {h}:{i}'">{{ scope.row.rectifyTime }}</span>
          </span>
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

import { problemTypeEnum } from '@enum-ms/production'
import { improveStatusEnum } from '@enum-ms/mes'
import { qhseDisclosurePM as permission } from '@/page-permission/mes'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

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
    v.qualityTypeListStr = v.qualityTypeList && v.qualityTypeList.join('、')
    v.imgUrls = v.attachmentDTOS && v.attachmentDTOS.map((o) => o.imageUrl)
    if (v.qhseRectifyDTOS && v.qhseRectifyDTOS.length) {
      v.rectifyName = v.qhseRectifyDTOS[0]?.name
      v.rectifyTime = v.qhseRectifyDTOS[0]?.createTime
      v.rectifyAttachmentDTOS = v.qhseRectifyDTOS[0]?.attachmentDTOS
      v.rectifyImgUrls = v.rectifyAttachmentDTOS && v.rectifyAttachmentDTOS.map((o) => o.imageUrl)
    }
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
