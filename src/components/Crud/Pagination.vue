<!--分页-->
<template>
  <el-pagination
    v-model:page-size="page.size"
    v-model:current-page="page.page"
    :total="page.total"
    style="margin-top: 8px;"
    layout="total, prev, pager, next, sizes"
    @size-change="crud.sizeChangeHandler($event)"
    @current-change="crud.pageChangeHandler"
  />
</template>

<script setup>
import { regPagination } from '@compos/use-crud'

const { crud, page } = regPagination()
</script>
<style lang="scss" scoped>
.el-pagination ::v-deep(.btn-prev),
.el-pagination ::v-deep(.btn-next) {
  margin: 1px !important;
}
::v-deep.el-pager li {
  margin: 1px !important;
}

</style>
