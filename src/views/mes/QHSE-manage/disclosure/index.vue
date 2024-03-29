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
      :data-format="dataFormat"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      :show-empty-symbol="false"
      style="width: 100%"
      @row-dblclick="handleDblRowClick"
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
        <template #default="{ row: { sourceRow: row } }">
          <span>{{ problemTypeEnum.VL[row.type] }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('serialNumber')"
        :show-overflow-tooltip="true"
        prop="serialNumber"
        label="构件编号"
        align="center"
        width="100px"
      >
        <template #default="{ row }">
          <span>{{ row.serialNumber || '/' }}</span>
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
        <template #default="{ row }">
          <span style="white-space: pre-line">
            <span>{{ row.userName + '\n' }}</span>
            <span>{{ row.createTime }}</span>
          </span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('project.shortName')"
        prop="project.shortName"
        :show-overflow-tooltip="true"
        label="所属项目"
        min-width="250"
      >
        <template #default="{ row: { sourceRow: row } }">
          <span class="project-name">{{ projectNameFormatter(row.project) }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('qualityTypeListStr')"
        :show-overflow-tooltip="true"
        prop="qualityTypeListStr"
        label="问题"
        min-width="100px"
      >
        <template #default="{ row }">
          <span>{{ row.qualityTypeListStr }}</span>
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
        <template #default="{ row: { sourceRow: row } }">
          <div class="imgs-box">
            <el-image
              v-for="url in row.attachmentDTOS"
              :preview-src-list="row.imgUrls"
              :initial-index="1"
              :key="url.id"
              :src="url.tinyImageUrl"
              scroll-container=".el-table__body-wrapper"
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
        <template #default="{ row: { sourceRow: row } }">
          <el-tag v-if="improveStatusEnum.VK[row.status]" :type="improveStatusEnum.V[row.status].T">{{
            improveStatusEnum.VL[row.status]
          }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('rectifyAttachmentDTOS')"
        prop="rectifyAttachmentDTOS"
        :show-overflow-tooltip="false"
        label="整改图片"
        width="150px"
        align="left"
      >
        <template #default="{ row: { sourceRow: row } }">
          <div class="imgs-box">
            <el-image
              v-for="url in row.rectifyAttachmentDTOS"
              :preview-src-list="row.rectifyImgUrls"
              :initial-index="1"
              :key="url.id"
              :src="url.tinyImageUrl"
              scroll-container=".el-table__body-wrapper"
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
        <template #default="{ row }">
          <span style="white-space: pre-line">
            <span>{{ row.rectifyName }}</span
            >{{ '\n' }}
            <span>{{ row.rectifyTime }}</span>
          </span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('forfeit')" :show-overflow-tooltip="true" prop="forfeit" label="罚金(元)" align="center">
        <template #default="{ row }">
          <span>{{ row.forfeit }}</span>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <!-- 详情 -->
    <detail v-model:visible="drawerVisible" :detail-data="detailData" />
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/QHSE-manage/disclosure'
import { ref } from 'vue'

import { projectNameFormatter } from '@/utils/project'
import { problemTypeEnum } from '@enum-ms/production'
import { improveStatusEnum } from '@enum-ms/mes'
import { qhseDisclosurePM as permission } from '@/page-permission/mes'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import detail from './module/detail.vue'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const dataFormat = ref([
  ['createTime', 'parse-time'],
  ['rectifyTime', 'parse-time'],
  ['forfeit', ['to-fixed-ck', 'YUAN']]
])

const tableRef = ref()
const detailData = ref({})
const drawerVisible = ref(false)
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

CRUD.HOOK.beforeToQuery = () => {
  // 只有质量问题需要传projectId
  if (crud.query.type !== problemTypeEnum.QUALITY.V) {
    crud.query.projectId = undefined
  }
}

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

function handleDblRowClick(val) {
  console.log(val, 'val')
  drawerVisible.value = true
  detailData.value = val
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
